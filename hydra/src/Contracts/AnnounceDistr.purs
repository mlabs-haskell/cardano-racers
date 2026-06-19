module CardanoRacers.Hydra.Contracts.AnnounceDistr
  ( announceRewardDistribution
  , mkAnnounceRewardDistributionTx
  ) where

import Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.Provider (ServerConfig)
import Cardano.Provider.ServerConfig (mkHttpUrl)
import Cardano.ToData (toData)
import Cardano.Types
  ( Address
  , Credential(ScriptHashCredential)
  , NetworkId
  , PlutusData
  , RedeemerDatum
  , ScriptHash
  , Transaction
  , UtxoMap
  , Value
  )
import Cardano.Types.Address (getPaymentCredential, mkPaymentAddress)
import Cardano.Types.Credential (asPubKeyHash)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Transaction (hash) as Transaction
import CardanoRacers.Hydra.Contracts.Common (findCollateralUtxo, findRaceStateUtxo, fixTx)
import CardanoRacers.Hydra.Lib.Transaction
  ( appendTxSignatures
  , removeTxOutputsWithEmptyValues
  , setExUnitsToMax
  , setTxValid
  )
import CardanoRacers.Hydra.Monad
  ( AppM
  , RaceData
  , RaceEntry
  , liftContract
  , liftContractNullCosts
  , readHydraSnapshot
  )
import CardanoRacers.Hydra.Services.HydraPeer (signAnnounceDistrTxRequest)
import CardanoRacers.Hydra.Types.Common (Utxo)
import CardanoRacers.Hydra.Types.ContractResult (buildTx, emptySubmitTxData)
import CardanoRacers.Hydra.Types.ServerResponse
  ( ServerResponse(ServerResponseError, ServerResponseSuccess)
  )
import CardanoRacers.Race.Types
  ( RaceDatum(RaceState)
  , RaceRedeemer(MoveL2)
  , RewardDistribution
  )
import Contract.Address (getNetworkId)
import Contract.BalanceTxConstraints (BalancerConstraints)
import Contract.BalanceTxConstraints
  ( mustSendChangeToAddress
  , mustUseAdditionalUtxos
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddresses
  ) as BalancerConstraints
import Contract.CborBytes (cborBytesToHex)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftedM)
import Contract.Prelude (mconcat)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.Transaction (signTransaction)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustBeSignedBy
  , mustNotBeValid
  , mustPayToScript
  , mustSpendScriptOutput
  ) as Constraints
import Contract.Wallet (getWalletAddress)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader.Class (ask)
import Control.Parallel (parTraverse)
import Data.Array (find) as Array
import Data.Either (either)
import Data.Foldable (foldMap)
import Data.Map (fromFoldable, toUnfoldable, union) as Map
import Data.Maybe (Maybe(Just, Nothing), isJust, isNothing, maybe)
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Debug (traceM)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Types (toUtxoMap)

announceRewardDistribution
  :: HydraNodeApiWebSocket AppM
  -> RaceData
  -> RewardDistribution
  -> AppM Unit
announceRewardDistribution ws raceData distr = do
  ownAddress <- liftContract $ liftedM "Could not get wallet address" getWalletAddress
  tx <- mkAnnounceRewardDistributionTx raceData ownAddress distr
  { config: { hydraNodeStartupParams: { peers } } } <- ask
  let raceCs = (unwrap raceData.raceParams).stateCurrencySymbol
  peerSignedTx <- liftAff $ multiSignAnnounceDistrTx peers raceCs tx ownAddress
  signedTx <- liftContract $ signTransaction peerSignedTx
  liftEffect $ ws.decommit signedTx
  logInfo' "Successfully signed and submitted AnnounceRewardDistribution Tx"

multiSignAnnounceDistrTx
  :: forall (r :: Row Type)
   . Array { httpServer :: ServerConfig | r }
  -> ScriptHash
  -> Transaction
  -> Address
  -> Aff Transaction
multiSignAnnounceDistrTx peers raceCs tx changeAddress = do
  signatures <- parTraverse
    ( \{ httpServer } -> do
        eiResp <-
          signAnnounceDistrTxRequest (mkHttpUrl httpServer)
            { raceCs
            , tx
            , changeAddress
            }
        resp <-
          either
            ( throwError <<< error
                <<< append "multiSignAnnounceDistrTx: signAnnounceDistrTxRequest failed: "
                <<< show
            )
            pure
            eiResp
        case resp of
          ServerResponseSuccess signature ->
            pure signature
          ServerResponseError err ->
            throwError $ error $
              "multiSignAnnounceDistrTx: failed to get signature from peer: " <>
                show err
    )
    peers
  pure $ appendTxSignatures signatures tx

mkAnnounceRewardDistributionTx :: RaceData -> Address -> RewardDistribution -> AppM Transaction
mkAnnounceRewardDistributionTx raceData changeAddress distr = do
  snapshotUtxos <- do
    snapshot <- readHydraSnapshot
    let utxos = toUtxoMap (unwrap snapshot).utxo
    pure $ maybe utxos (Map.union utxos <<< toUtxoMap)
      (unwrap snapshot).utxoToCommit
  tx <- liftContractNullCosts $ announceRewardDistributionContract snapshotUtxos changeAddress
    raceData
    distr
  logInfo' $ "Successfully built AnnounceRewardDistribution Tx with hash: " <>
    cborBytesToHex (encodeCbor $ Transaction.hash tx)
  pure tx

announceRewardDistributionContract
  :: UtxoMap
  -> Address
  -> RaceData
  -> RewardDistribution
  -> Contract Transaction
announceRewardDistributionContract snapshotUtxos changeAddress raceData distr = do
  let raceValidatorHash = PlutusScript.hash raceData.raceValidator
  network <- getNetworkId

  traceM $ "Snapshot utxos: " <> show snapshotUtxos

  collateralUtxo <- liftMaybe (error "Could not find collateral utxo") $
    findCollateralUtxo snapshotUtxos

  { utxo: raceStateUtxo@(raceStateOref /\ raceStateOut), distr: oldDistr } <-
    liftMaybe (error "Could not find RaceState utxo") $
      findRaceStateUtxo raceValidatorHash snapshotUtxos

  unless (isNothing oldDistr) do
    throwError $ error "Reward distribution already provided"

  let
    utxos :: UtxoMap
    utxos = Map.fromFoldable [ collateralUtxo, raceStateUtxo ]

    redeemer :: RedeemerDatum
    redeemer = wrap $ toData MoveL2

    datum :: PlutusData
    datum = toData $ RaceState { distribution: Just distr }

    raceStateValue :: Value
    raceStateValue = (unwrap raceStateOut).amount

    balancerConstraints :: BalancerConstraints
    balancerConstraints = mconcat
      [ BalancerConstraints.mustUseUtxosAtAddresses mempty
      , BalancerConstraints.mustUseCollateralUtxos $ Map.fromFoldable [ collateralUtxo ]
      , BalancerConstraints.mustUseAdditionalUtxos utxos
      , BalancerConstraints.mustSendChangeToAddress changeAddress
      ]

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustSpendScriptOutput raceStateOref redeemer
      , Constraints.mustPayToScript raceValidatorHash datum DatumInline raceStateValue
      , foldMap (Constraints.mustBeSignedBy <<< wrap) (unwrap raceData.raceParams).delegates
      , Constraints.mustNotBeValid
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs utxos
      , Lookups.validator raceData.raceValidator
      ]

  tx <- buildTx $ emptySubmitTxData
    { lookups = lookups
    , constraints = constraints
    , balancerConstraints = balancerConstraints
    }
  let validTx = setTxValid $ removeTxOutputsWithEmptyValues tx
  evaluatedTx <- setExUnitsToMax validTx
  pure evaluatedTx
