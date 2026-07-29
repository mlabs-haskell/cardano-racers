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
import CardanoRacers.Hydra.Contracts.Common (fixTx)
import CardanoRacers.Hydra.Lib.Transaction (appendTxSignatures)
import CardanoRacers.Hydra.Monad (AppM, getHydraNodeBaseUrl, liftContract)
import CardanoRacers.Hydra.Services.HydraPeer (signCommitTxRequest)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Race.Contract (mkRaceValidator)
import CardanoRacers.Race.Types (RaceParams, RaceRedeemer(MoveL2))
import Contract.Address (getNetworkId)
import Contract.Monad (Contract)
import Contract.Prelude (mconcat)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Transaction (submit)
import Contract.TxConstraints (TxConstraints)
import Contract.TxConstraints (mustBeSignedBy, mustSpendScriptOutput) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTx)
import Contract.Wallet (ownPaymentPubKeyHash)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader (ask)
import Control.Parallel (parTraverse)
import Data.Either (Either(Left, Right), either)
import Data.Foldable (foldMap)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import HydraSdk.NodeApi (commitRequest)
import HydraSdk.Types (HydraCommitRequest, mkFullCommitRequest, mkSimpleCommitRequest)
import Racers (runRacers)

commitCollateralToHydra :: AppM TransactionHash
commitCollateralToHydra = do
  { collateralUtxo } <- ask
  collateralUtxo' <- liftMaybe (error "commitCollateralToHydra: collateralUtxo is Nothing") $
    collateralUtxo
  let req = mkSimpleCommitRequest $ Map.fromFoldable [ collateralUtxo' ]
  hydraNodeBaseUrl <- getHydraNodeBaseUrl
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeBaseUrl
    liftContract $ fixTx tx [ PlutusV3 ]
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
  { config: { hydraNodeStartupParams: { peers } } } <- ask
  { tx: blueprintTx, raceValidator } <- liftContract $ mkBlueprintTx rp raceParams raceUtxo
  let req = mkFullCommitRequest blueprintTx $ Map.fromFoldable [ raceUtxo ]
  hydraNodeBaseUrl <- getHydraNodeBaseUrl
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeBaseUrl
    liftContract $ fixTx tx [ PlutusV2 ]
  pkh <-
    liftMaybe (error "commitRaceUtxoToHydra: could not get own pkh") =<<
      liftContract ownPaymentPubKeyHash
  signedCommitTx <- liftAff $ multiSignCommitTx peers commitTx (unwrap pkh) rp raceParams
  txHash <- liftContract $ submit signedCommitTx
  pure { txHash, raceValidator }

queryCommitTx :: HydraCommitRequest -> String -> Aff Transaction
queryCommitTx req hydraNodeBaseUrl = do
  eiResult <- commitRequest hydraNodeBaseUrl req
  hydraTx <-
    either (throwError <<< error <<< append "queryCommitTx: commitRequest failed: " <<< show)
      pure
      eiResult
  commitTx <-
    liftMaybe (error "queryCommitTx: could not decode transaction") $
      decodeCbor hydraTx.cborHex
  pure commitTx

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
        resp <-
          signCommitTxRequest (mkHttpUrl httpServer)
            { commitTx
            , commitLeader: pkh
            , racersParams
            , raceParams: encodeCbor $ toData raceParams
            }
        case resp of
          Left httpErr ->
            throwError $ error $ "multiSignCommitTx: signCommitTx request failed with error: "
              <> show httpErr
          Right (Left domainErr) ->
            throwError $ error $ "multiSignCommitTx: signCommitTx endpoint returned error: "
              <> show domainErr
          Right (Right sig) ->
            pure sig
    )
    peers
  pure $ appendTxSignatures signatures commitTx

mkBlueprintTx
  :: RacersParams
  -> RaceParams
  -> Utxo
  -> Contract
       { tx :: Transaction
       , raceValidator :: PlutusScript
       }
mkBlueprintTx rp raceParams raceUtxo = do
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
      , foldMap (Constraints.mustBeSignedBy <<< wrap) (unwrap raceParams).delegates
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable [ raceUtxo ]
      , Lookups.validator raceValidator
      ]

  blueprintTx /\ _usedUtxos <- mkUnbalancedTx lookups constraints
  pure { tx: blueprintTx, raceValidator }
