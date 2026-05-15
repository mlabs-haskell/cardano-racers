module CardanoRacers.Race.Contract
  ( distributeRewards
  , mkRaceValidator
  , startRace
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Map (toCardano) as Plutus.Map
import Cardano.Plutus.Types.Value (fromCardano, toCardano) as Plutus.Value
import Cardano.ToData (toData)
import Cardano.Types
  ( Address
  , Asset(Asset)
  , AssetName
  , Credential(ScriptHashCredential)
  , Ed25519KeyHash
  , PlutusScript
  , RedeemerDatum
  , ScriptHash
  , TransactionHash
  , TransactionInput
  , TransactionOutput
  , Value
  )
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.BigNum (fromInt, one, toBigInt) as BigNum
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.Value (lovelaceValueOf, valueOf, valueToCoin)
import Cardano.Types.Value (singleton) as Value
import CardanoRacers.Helpers (mkPosixTimeUnsafe, paysToAddrConstraint)
import CardanoRacers.Race.Types
  ( RaceDatum(ValueEscrow, RaceState, TokenBin)
  , RaceParams
  , RaceRedeemer(DistributeRewards)
  , RewardDistribution
  , raceStateTokenName
  , valueEscrowTokenName
  )
import CardanoRacers.RaceSlot.Contract
  ( mintRaceSlotTokenConstraints
  , mkRaceSlotPolicy
  )
import CardanoRacers.RaceSlot.Types (RaceHash)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import CardanoRacers.ScriptsFFI (raceScript)
import CardanoRacers.Types.FixedDecimal (FixedDecimal, N5)
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (Contract, liftContractM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustPayToScript
  , mustReferenceOutput
  , mustSpendScriptOutput
  ) as Constraints
import Contract.Utxos (utxosAt)
import Control.Monad.Error.Class (liftMaybe, throwError)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head) as Array
import Data.Bifunctor (lmap)
import Data.Int (ceil)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Newtype (wrap)
import Data.Time.Duration (Days(Days))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Effect.Exception (error)
import JS.BigInt (toNumber) as BigInt
import Partial.Unsafe (unsafePartial)
import Racers (Racers)

startRace
  :: Maybe RewardDistribution -- should only be set in tests
  -> RaceHash
  -> Value
  -> Array (FixedDecimal N5)
  -> Array Plutus.Address
  -> Array Ed25519KeyHash
  -> Racers
       { txHash :: TransactionHash
       , raceParams :: RaceParams
       }
startRace
  distribution
  raceHash
  totalRewardValue
  rewardWeights
  participants
  delegates = do
  slotPolicy <- mkRaceSlotPolicy raceHash

  slotPolicyHash <-
    lift $ liftContractM "Could not get race slot token script hash"
      ( PlutusScript.hash <$>
          Array.head (unwrap slotPolicy).plutusMintingPolicies
      )

  (slotConstraints /\ slotLookups) <- mintRaceSlotTokenConstraints
    raceHash
    [ raceStateTokenName /\ one
    , valueEscrowTokenName /\ one
    ]

  nowTime <- lift currentTime

  let
    raceParams :: RaceParams
    raceParams = wrap
      { stateCurrencySymbol: slotPolicyHash
      , totalRewardValue: Plutus.Value.fromCardano totalRewardValue
      , participants
      , delegates
      , escrowTtl: nowTime + mkPosixTimeUnsafe (Days 2.0)
      , rewardWeights
      }

  raceValidatorHash <- PlutusScript.hash <$> mkRaceValidator raceParams
  let
    mkStateTokenValue :: AssetName -> Value
    mkStateTokenValue tn = Value.singleton slotPolicyHash tn BigNum.one

    raceStateTokenValue :: Value
    raceStateTokenValue = mkStateTokenValue raceStateTokenName

    valueEscrowTokenValue :: Value
    valueEscrowTokenValue = mkStateTokenValue valueEscrowTokenName

    raceStateValue :: Value
    raceStateValue =
      unsafePartial $ raceStateTokenValue <> lovelaceValueOf
        (BigNum.fromInt 5_000_000)

    escrowValue :: Value
    escrowValue = unsafePartial $ totalRewardValue <> valueEscrowTokenValue

    -- TODO: ensure totalRewardValue comes from the treasury wallet?
    constraints :: TxConstraints
    constraints = mconcat
      [ slotConstraints

      , Constraints.mustPayToScript raceValidatorHash (toData ValueEscrow)
          DatumInline
          escrowValue

      , Constraints.mustPayToScript raceValidatorHash
          (toData $ RaceState { distribution })
          DatumInline
          raceStateValue
      ]

    lookups :: ScriptLookups
    lookups = slotLookups

  lift do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    pure
      { txHash
      , raceParams
      }

distributeRewards :: RaceParams -> Racers TransactionHash
distributeRewards raceParams = do
  RacersState { treasuryAddress, operatingAddress } /\ racersStateOref /\
    racersStateTxOut <- queryRacersState

  network <- lift getNetworkId
  raceValidator <- mkRaceValidator raceParams
  let
    stateCurrencySymbol = (unwrap raceParams).stateCurrencySymbol
    raceValidatorHash = PlutusScript.hash raceValidator
    raceValidatorAddress =
      mkPaymentAddress network (wrap $ ScriptHashCredential raceValidatorHash)
        Nothing

  { raceStateUtxo, valueEscrowUtxo } <- lift $ findRaceStateAndValueEscrowUtxos
    raceValidatorAddress
    stateCurrencySymbol

  raceStateDatum <-
    liftMaybe (error "Could not decode RaceDatum") $
      decodeRaceDatum (snd raceStateUtxo)
  rewardDistr <-
    case raceStateDatum of
      RaceState { distribution: Just distr } ->
        liftMaybe (error "Could not convert Plutus distribution to Map") $
          Plutus.Map.toCardano distr
      RaceState { distribution: Nothing } ->
        throwError $ error "Reward distribution not announced"
      _ ->
        throwError $ error
          $ "Unexpected RaceDatum variant. Expected: RaceState, datum: "
          <> show raceStateDatum

  (rewards :: Array (Plutus.Address /\ Value)) <-
    traverse
      ( \(addr /\ plutusReward) ->
          case Plutus.Value.toCardano plutusReward of
            Just reward ->
              pure $ addr /\ reward
            Nothing -> do
              throwError $ error
                $ "Could not convert Plutus reward to Cardano.Value: "
                <> show plutusReward
      )
      (Map.toUnfoldable rewardDistr)

  let
    mkStateTokenValue :: AssetName -> Value
    mkStateTokenValue tn = Value.singleton stateCurrencySymbol tn BigNum.one

    stateTokens :: Value
    stateTokens =
      unsafePartial
        ( mkStateTokenValue raceStateTokenName
            <> mkStateTokenValue valueEscrowTokenName
        )

    -- TODO: ensure these calculations are consistent with the on-chain validator
    raceStateLovelace :: Number
    raceStateLovelace = BigInt.toNumber $ BigNum.toBigInt $ unwrap $ valueToCoin
      (unwrap $ snd raceStateUtxo).amount

    treasuryValue :: Value
    treasuryValue =
      lovelaceValueOf $ BigNum.fromInt $ ceil $ raceStateLovelace * 0.75

    operatingValue :: Value
    operatingValue =
      lovelaceValueOf $ BigNum.fromInt $ ceil $ raceStateLovelace * 0.25

    redeemer :: RedeemerDatum
    redeemer = wrap $ toData DistributeRewards

    constraints :: TxConstraints
    constraints = mconcat
      [ Constraints.mustSpendScriptOutput (fst raceStateUtxo) redeemer
      , Constraints.mustSpendScriptOutput (fst valueEscrowUtxo) redeemer
      , foldMap (uncurry paysToAddrConstraint) rewards
      , Constraints.mustPayToScript raceValidatorHash (toData TokenBin)
          DatumInline
          stateTokens
      , Constraints.mustReferenceOutput racersStateOref
      , paysToAddrConstraint treasuryAddress treasuryValue
      , paysToAddrConstraint operatingAddress operatingValue
      ]

    lookups :: ScriptLookups
    lookups = mconcat
      [ Lookups.unspentOutputs $ Map.fromFoldable
          [ raceStateUtxo
          , valueEscrowUtxo
          , racersStateOref /\ racersStateTxOut
          ]
      , Lookups.validator raceValidator
      ]

  lift do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    pure txHash

findRaceStateAndValueEscrowUtxos
  :: Address
  -> ScriptHash
  -> Contract
       { raceStateUtxo :: TransactionInput /\ TransactionOutput
       , valueEscrowUtxo :: TransactionInput /\ TransactionOutput
       }
findRaceStateAndValueEscrowUtxos scriptAddr stateCs = do
  scriptUtxos <- Map.toUnfoldable <$> utxosAt scriptAddr
  raceStateUtxo <-
    liftMaybe (error "Could not find RaceState utxo") $
      Array.find
        ( \(_ /\ txOut) ->
            valueOf (Asset stateCs raceStateTokenName) (unwrap txOut).amount
              == BigNum.one
        )
        scriptUtxos
  valueEscrowUtxo <-
    liftMaybe (error "Could not find ValueEscrow utxo") $
      Array.find
        ( \(_ /\ txOut) ->
            valueOf (Asset stateCs valueEscrowTokenName) (unwrap txOut).amount
              == BigNum.one
        )
        scriptUtxos
  pure
    { raceStateUtxo
    , valueEscrowUtxo
    }

decodeRaceDatum :: TransactionOutput -> Maybe RaceDatum
decodeRaceDatum txOut =
  case (unwrap txOut).datum of
    Just datum ->
      case outputDatumDatum datum of
        Just inlineDatum -> fromData inlineDatum
        Nothing -> Nothing
    Nothing -> Nothing

mkRaceValidator :: RaceParams -> Racers PlutusScript
mkRaceValidator params = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceScript
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ lmap (error <<< show) $ applyArgs v2script
    [ toData rp
    , toData params
    ]
  pure appliedScript
