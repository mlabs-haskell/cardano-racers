module CardanoRacers.Hydra.Contracts.AnnounceDistr
  ( announceRewardDistribution
  , mkAnnounceRewardDistributionTx
  ) where

import Prelude

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
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import CardanoRacers.Hydra.Contracts.Collateral (isCollateralTxOut)
import CardanoRacers.Hydra.Lib.Transaction (appendTxSignatures, setExUnitsToMax, setTxValid)
import CardanoRacers.Hydra.Monad
  ( AppM
  , RaceData
  , getHydraUtxos
  , liftContract
  , liftContractNullCosts
  , readRaceData
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
  ( mustUseAdditionalUtxos
  , mustUseCollateralUtxos
  , mustUseUtxosAtAddresses
  ) as BalancerConstraints
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
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (unwrap, wrap)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import HydraSdk.NodeApi (HydraNodeApiWebSocket)

announceRewardDistribution :: HydraNodeApiWebSocket AppM -> RewardDistribution -> AppM Unit
announceRewardDistribution ws distr = do
  ownAddress <- liftContract $ liftedM "Could not get wallet address" getWalletAddress
  tx <- mkAnnounceRewardDistributionTx ownAddress distr
  { config: { hydraNodeStartupParams: { peers } } } <- ask
  peerSignedTx <- liftAff $ multiSignAnnounceDistrTx peers tx ownAddress
  signedTx <- liftContract $ signTransaction peerSignedTx
  liftEffect $ ws.submitTxL2 signedTx

multiSignAnnounceDistrTx
  :: forall (r :: Row Type)
   . Array { httpServer :: ServerConfig | r }
  -> Transaction
  -> Address
  -> Aff Transaction
multiSignAnnounceDistrTx peers tx collateralAddress = do
  signatures <- parTraverse
    ( \{ httpServer } -> do
        eiResp <-
          signAnnounceDistrTxRequest (mkHttpUrl httpServer)
            { tx
            , collateralAddress
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
          ServerResponseError signCommitTxErr ->
            throwError $ error $
              "multiSignAnnounceDistrTx: failed to get signature from peer: " <>
                show signCommitTxErr
    )
    peers
  pure $ appendTxSignatures signatures tx

mkAnnounceRewardDistributionTx :: Address -> RewardDistribution -> AppM Transaction
mkAnnounceRewardDistributionTx collateralAddr distr = do
  snapshotUtxos <- getHydraUtxos
  raceData <- readRaceData
  liftContractNullCosts $ announceRewardDistributionContract snapshotUtxos collateralAddr
    raceData
    distr

announceRewardDistributionContract
  :: UtxoMap
  -> Address
  -> RaceData
  -> RewardDistribution
  -> Contract Transaction
announceRewardDistributionContract snapshotUtxos collateralAddr raceData distr = do
  let raceValidatorHash = PlutusScript.hash raceData.raceValidator
  network <- getNetworkId

  collateralUtxo <- liftMaybe (error "Could not find collateral utxo") $
    findCollateralUtxo snapshotUtxos collateralAddr

  raceStateUtxo@(raceStateOref /\ raceStateOut) <-
    liftMaybe (error "Could not find RaceState utxo") $
      findRaceStateUtxo network snapshotUtxos raceValidatorHash

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
  let validTx = setTxValid tx
  evaluatedTx <- setExUnitsToMax validTx
  pure evaluatedTx

findCollateralUtxo :: UtxoMap -> Address -> Maybe Utxo
findCollateralUtxo utxos addr =
  Array.find
    (\(_ /\ txOut) -> isCollateralTxOut txOut && (unwrap txOut).address == addr)
    (Map.toUnfoldable utxos)

findRaceStateUtxo :: NetworkId -> UtxoMap -> ScriptHash -> Maybe Utxo
findRaceStateUtxo network utxos sh =
  Array.find
    ( \(_ /\ txOut) -> (unwrap txOut).address == mkPaymentAddress network
        (wrap $ ScriptHashCredential sh)
        Nothing
    )
    (Map.toUnfoldable utxos)
