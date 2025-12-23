module CardanoRacers.Hydra.Contracts.Commit
  ( commitCollateralToHydra
  , commitRaceUtxoToHydra
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor)
import Cardano.Provider (ServerConfig)
import Cardano.Provider.ServerConfig (mkHttpUrl)
import Cardano.ToData (toData)
import Cardano.Types
  ( Ed25519KeyHash
  , RedeemerDatum
  , Transaction
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  )
import CardanoRacers.Hydra.Lib.Transaction (appendTxSignatures)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Services.HydraPeer (signCommitTxRequest)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  )
import CardanoRacers.Race.Contract (mkRaceValidator)
import CardanoRacers.Race.Types (RaceParams, RaceRedeemer(MoveL2))
import Contract.Log (logDebug')
import Contract.Monad (Contract)
import Contract.Prelude (mconcat)
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
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Exception (error)
import HydraSdk.NodeApi (commitRequest)
import HydraSdk.Process (HydraHeadPeer)
import HydraSdk.Types
  ( HostPort
  , HydraCommitRequest
  , mkFullCommitRequest
  , mkSimpleCommitRequest
  )
import URI.Port (toInt) as Port

commitCollateralToHydra :: AppM TransactionHash
commitCollateralToHydra = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress } } } <- ask
  let req = mkSimpleCommitRequest $ Map.fromFoldable [ collateralUtxo ]
  commitTx <- liftAff $ queryCommitTx req hydraNodeApiAddress
  liftContract $ submit commitTx

commitRaceUtxoToHydra :: Utxo -> RaceParams -> AppM TransactionHash
commitRaceUtxoToHydra raceUtxo raceParams = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress, peers } } } <- ask
  blueprintTx <- liftContract $ mkBlueprintTx raceParams raceUtxo collateralUtxo
  let req = mkFullCommitRequest blueprintTx $ Map.fromFoldable [ raceUtxo, collateralUtxo ]
  commitTx <- liftAff $ queryCommitTx req hydraNodeApiAddress
  pkh <-
    liftMaybe (error "commitRaceUtxoToHydra: could not get own pkh") =<<
      liftContract ownPaymentPubKeyHash
  signedCommitTx <- liftAff $ multiSignCommitTx peers commitTx $ unwrap pkh
  liftContract $ submit signedCommitTx

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

multiSignCommitTx
  :: forall (r :: Row Type)
   . Array { httpServer :: ServerConfig | r }
  -> Transaction
  -> Ed25519KeyHash
  -> Aff Transaction
multiSignCommitTx peers commitTx pkh = do
  signatures <- parTraverse
    ( \{ httpServer } -> do
        eiResp <-
          signCommitTxRequest (mkHttpUrl httpServer)
            { commitTx
            , commitLeader: pkh
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

mkBlueprintTx :: RaceParams -> Utxo -> Utxo -> Contract Transaction
mkBlueprintTx raceParams raceUtxo collateralUtxo = do
  raceValidator <- mkRaceValidator raceParams
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
  pure blueprintTx
