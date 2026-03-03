module CardanoRacers.Hydra.Contracts.Commit
  ( commitCollateralToHydra
  , commitRaceUtxoToHydra
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Provider (ServerConfig)
import Cardano.Provider.ServerConfig (mkHttpUrl)
import Cardano.ToData (toData)
import Cardano.Types
  ( Credential(ScriptHashCredential)
  , Ed25519KeyHash
  , Language(PlutusV2, PlutusV3)
  , PlutusScript
  , RedeemerDatum
  , Transaction
  , TransactionHash
  )
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Hydra.Lib.Transaction
  ( appendTxSignatures
  , reSignTransaction
  , setAuxDataHash
  )
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Services.HydraPeer (signCommitTxRequest)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  )
import CardanoRacers.Race.Contract (mkRaceValidator)
import CardanoRacers.Race.Types (RaceParams, RaceRedeemer(MoveL2))
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.Prelude (mconcat)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Transaction (submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustBeSignedBy, mustSpendPubKeyOutput, mustSpendScriptOutput) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse)
import Ctl.Internal.Transaction (setScriptDataHash)
import Data.Array (elem) as Array
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Map (filterKeys, fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Data.UInt (fromInt) as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraSdk.NodeApi (commitRequest)
import HydraSdk.Types
  ( HostPort
  , HydraCommitRequest
  , mkFullCommitRequest
  , mkSimpleCommitRequest
  )
import Racers (runRacers)
import URI.Port (toInt) as Port

commitCollateralToHydra :: AppM TransactionHash
commitCollateralToHydra = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress } } } <- ask
  let req = mkSimpleCommitRequest $ Map.fromFoldable [ collateralUtxo ]
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeApiAddress
    liftContract $ fixCommitTx tx [ PlutusV3 ]
  liftContract $ submit commitTx

commitRaceUtxoToHydra
  :: Utxo
  -> RacersParams
  -> RaceParams
  -> AppM
       { txHash :: TransactionHash
       , raceValidator :: PlutusScript
       }
commitRaceUtxoToHydra raceUtxo rp raceParams = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress, peers } } } <- ask
  { tx: blueprintTx, raceValidator } <- liftContract $ mkBlueprintTx rp raceParams raceUtxo
    collateralUtxo
  let req = mkFullCommitRequest blueprintTx $ Map.fromFoldable [ raceUtxo, collateralUtxo ]
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeApiAddress
    liftContract $ fixCommitTx tx [ PlutusV2, PlutusV3 ]
  pkh <-
    liftMaybe (error "commitRaceUtxoToHydra: could not get own pkh") =<<
      liftContract ownPaymentPubKeyHash
  signedCommitTx <- liftAff $ multiSignCommitTx peers commitTx (unwrap pkh) rp raceParams
  txHash <- liftContract $ submit signedCommitTx
  pure { txHash, raceValidator }

queryCommitTx :: HydraCommitRequest -> HostPort -> Aff Transaction
queryCommitTx req hydraNodeApiAddress = do
  eiResult <- commitRequest (mkHttpUrl hydraNodeApiServerConfig) req
  hydraTx <-
    either (throwError <<< error <<< append "queryCommitTx: commitRequest failed: " <<< show)
      pure
      eiResult
  commitTx <-
    liftMaybe (error "queryCommitTx: could not decode transaction") $
      decodeCbor hydraTx.cborHex
  pure commitTx
  where
  hydraNodeApiServerConfig :: ServerConfig
  hydraNodeApiServerConfig =
    { port: UInt.fromInt $ Port.toInt hydraNodeApiAddress.port
    , host: "127.0.0.1"
    , secure: false
    , path: Nothing
    }

-- Recompute script integrity and auxiliary data hashes, and re-sign the
-- transaction. Tx CBOR can change after re-serialization.
fixCommitTx :: Transaction -> Array Language -> Contract Transaction
fixCommitTx tx languages = do
  pparams <- unwrap <$> getProtocolParameters
  let
    costModels =
      -- PlutusV2 for CardanoRacers scripts, PlutusV3 for Hydra scripts
      Map.filterKeys (flip Array.elem languages)
        pparams.costModels
    ws = unwrap (unwrap tx).witnessSet
  fixedTx <- liftEffect $ setScriptDataHash costModels ws.redeemers ws.plutusData $
    setAuxDataHash tx
  signedTx <- reSignTransaction fixedTx
  pure signedTx

multiSignCommitTx
  :: forall (r :: Row Type)
   . Array { httpServer :: ServerConfig | r }
  -> Transaction
  -> Ed25519KeyHash
  -> RacersParams
  -> RaceParams
  -> Aff Transaction
multiSignCommitTx peers commitTx pkh racersParams raceParams = do
  signatures <- parTraverse
    ( \{ httpServer } -> do
        eiResp <-
          signCommitTxRequest (mkHttpUrl httpServer)
            { commitTx
            , commitLeader: pkh
            , racersParams
            , raceParams: encodeCbor $ toData raceParams
            }
        resp <-
          either
            ( throwError <<< error <<< append "multiSignCommitTx: signCommitTxRequest failed: "
                <<< show
            )
            pure
            eiResp
        case resp of
          ServerResponseSuccess signature ->
            pure signature
          ServerResponseError signCommitTxErr ->
            throwError $ error $ "multiSignCommitTx: failed to get signature from peer: " <>
              show signCommitTxErr
    )
    peers
  pure $ appendTxSignatures signatures commitTx

mkBlueprintTx
  :: RacersParams
  -> RaceParams
  -> Utxo
  -> Utxo
  -> Contract
       { tx :: Transaction
       , raceValidator :: PlutusScript
       }
mkBlueprintTx rp raceParams raceUtxo collateralUtxo = do
  raceValidator <- runRacers rp $ mkRaceValidator raceParams
  network <- getNetworkId
  let
    validatorHash = PlutusScript.hash raceValidator
    validatorAddr =
      mkPaymentAddress network (wrap $ ScriptHashCredential validatorHash) Nothing
    raceUtxoAddr = (unwrap $ snd raceUtxo).address

  when (validatorAddr /= raceUtxoAddr) do
    throwError $ error $ "mkBlueprintTx: race validator address mismatch. utxo address: "
      <> show raceUtxoAddr
      <> ", computed: "
      <> show validatorAddr

  let
    redeemer :: RedeemerDatum
    redeemer = wrap $ toData MoveL2

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustSpendScriptOutput (fst raceUtxo) redeemer
      , Constraints.mustSpendPubKeyOutput (fst collateralUtxo)
      , foldMap (Constraints.mustBeSignedBy <<< wrap) (unwrap raceParams).delegates
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable [ raceUtxo, collateralUtxo ]
      , Lookups.validator raceValidator
      ]

  blueprintTx /\ _usedUtxos <- mkUnbalancedTx lookups constraints
  pure { tx: blueprintTx, raceValidator }
