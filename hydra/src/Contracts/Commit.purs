module CardanoRacers.Hydra.Contracts.Commit
  ( commitCollateralToHydra
  , commitRaceUtxoToHydra
  ) where

import Prelude

import Cardano.AsCbor (decodeCbor, encodeCbor)
import Cardano.Provider (ServerConfig)
import Cardano.Provider.ServerConfig (mkHttpUrl)
import Cardano.ToData (toData)
import Cardano.Types (Credential(ScriptHashCredential), Ed25519KeyHash, Language(PlutusV2, PlutusV3), RedeemerDatum, Transaction, TransactionHash, TransactionInput, TransactionOutput)
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import CardanoRacers.Hydra.Lib.Transaction (appendTxSignatures, reSignTransaction, setAuxDataHash)
import CardanoRacers.Hydra.Monad (AppM, liftContract)
import CardanoRacers.Hydra.Services.HydraPeer (signCommitTxRequest)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.ServerResponse (ServerResponse(ServerResponseError, ServerResponseSuccess))
import CardanoRacers.Race.Contract (mkRaceValidator)
import CardanoRacers.Race.Types (RaceParams, RaceRedeemer(MoveL2))
import Contract.Address (getNetworkId)
import Contract.CborBytes (cborBytesToHex)
import Contract.Log (logDebug', logInfo')
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
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Map (empty, filterKeys, fromFoldable) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\), (/\))
import Data.UInt (fromInt) as UInt
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraSdk.NodeApi (commitRequest)
import HydraSdk.Process (HydraHeadPeer)
import HydraSdk.Types (HostPort, HydraCommitRequest, mkFullCommitRequest, mkSimpleCommitRequest)
import URI.Port (toInt) as Port

commitCollateralToHydra :: AppM TransactionHash
commitCollateralToHydra = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress } } } <- ask
  let req = mkSimpleCommitRequest $ Map.fromFoldable [ collateralUtxo ]
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeApiAddress
    liftContract $ fixCommitTx tx
  liftContract $ submit commitTx

-- FIXME
-- Hydra "Initial" validator fails for "Fixed CommitTx" with I11 error: ExpectedCommitDatumTypeGotSomethingElse
-- CBOR decoder bug in CDL?
--
-- To reproduce:
-- > test x = decodeCbor (encodeCbor x) == Just x
-- > test (List [(Bytes (hexToByteArrayUnsafe "d8799fd8799fd87a9f581ce3602e5cefef774ce26c05052dd4fd2544146cf4a3489d7d62e1ae2fffd87a80ffa240a1401a001081d2581ca7b88431985897177445eaf04881ba54c9e6d4cf3b3057535a40c6efa144536c6f7401d87b9fd87a9fd87a80ffffd87a80ff")),(Constr BigNum.zero []),(Integer one)])
-- false
--
-- https://github.com/cardano-scaling/hydra/blob/ba13df8d58a9020e4f8c5d4314d6ca19022ef788/hydra-plutus/validators/initial.ak#L94
-- https://github.com/mlabs-haskell/cardano-data-lite/blob/b099b3d11b009b971f918217bc15a769555be8fb/src/lib/cbor/reader.ts#L304
commitRaceUtxoToHydra :: Utxo -> RaceParams -> AppM TransactionHash
commitRaceUtxoToHydra raceUtxo raceParams = do
  { collateralUtxo, config: { hydraNodeStartupParams: { hydraNodeApiAddress, peers } } } <- ask
  blueprintTx <- liftContract $ mkBlueprintTx raceParams raceUtxo collateralUtxo
  let req = mkFullCommitRequest blueprintTx $ Map.fromFoldable [ raceUtxo, collateralUtxo ]
  commitTx <- do
    tx <- liftAff $ queryCommitTx req hydraNodeApiAddress
    liftContract $ fixCommitTx tx
  traceM $ "Fixed CommitTx: " <> cborBytesToHex (encodeCbor commitTx)
  liftContract do
    { provider } <- ask
    evalResult <- liftAff $ provider.evaluateTx commitTx Map.empty
    traceM $ "TxEval Result: " <> show evalResult
  pkh <-
    liftMaybe (error "commitRaceUtxoToHydra: could not get own pkh") =<<
      liftContract ownPaymentPubKeyHash
  -- FIXME
  -- signedCommitTx <- liftAff $ multiSignCommitTx peers commitTx $ unwrap pkh
  -- traceM $ "Multi-signed CommitTx: " <> cborBytesToHex (encodeCbor signedCommitTx)
  -- liftContract $ submit signedCommitTx
  liftContract $ submit commitTx

queryCommitTx :: HydraCommitRequest -> HostPort -> Aff Transaction
queryCommitTx req hydraNodeApiAddress = do
  eiResult <- commitRequest (mkHttpUrl hydraNodeApiServerConfig) req
  hydraTx <-
    either (throwError <<< error <<< append "queryCommitTx: commitRequest failed: " <<< show)
      pure
      eiResult
  traceM $ "Hydra CommitTx: " <> cborBytesToHex hydraTx.cborHex
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
fixCommitTx :: Transaction -> Contract Transaction
fixCommitTx tx = do
  pparams <- unwrap <$> getProtocolParameters
  let
    costModels =
      -- PlutusV2 for CardanoRacers scripts, PlutusV3 for Hydra scripts
      Map.filterKeys (\lang -> lang == PlutusV2 || lang == PlutusV3)
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
      -- FIXME: , foldMap (Constraints.mustBeSignedBy <<< wrap) (unwrap raceParams).delegates
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable [ raceUtxo, collateralUtxo ]
      , Lookups.validator raceValidator
      ]

  blueprintTx /\ _usedUtxos <- mkUnbalancedTx lookups constraints
  pure blueprintTx
