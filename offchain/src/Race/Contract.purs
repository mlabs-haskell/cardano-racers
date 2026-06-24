module CardanoRacers.Race.Contract
  ( distributeRewards
  , distributeRewardsReturningErrors
  , mkRaceValidator
  , startRace
  , startRaceWithHardcodedRewardDistribution
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Plutus.Types.Map (toCardano) as Plutus.Map
import Cardano.Plutus.Types.Value (fromCardano, toCardano) as Plutus.Value
import Cardano.ToData (toData)
import Cardano.Types
  ( Asset(Asset)
  , AssetName
  , Credential(ScriptHashCredential)
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
  ( DistributeRewardsContractError
      ( CouldNotFindRaceStateUtxo
      , CouldNotFindValueEscrowUtxo
      , CouldNotDecodeRaceDatum
      , CouldNotConvertDistribution
      , RewardDistributionNotAnnounced
      , UnexpectedRaceDatumVariant
      , CouldNotConvertRewardValue
      , CouldNotConvertFeePerDelegateValue
      )
  , RaceDatum(ValueEscrow, RaceState, TokenBin)
  , RaceParams
  , RaceRedeemer(DistributeRewards)
  , RewardDistribution
  , StartRaceParams(StartRaceParams)
  , StartRaceResult
  , raceStateTokenName
  , valueEscrowTokenName
  )
import CardanoRacers.RaceRegistry.Types (RaceParticipant(RaceParticipant))
import CardanoRacers.RaceSlot.Contract
  ( mintRaceSlotTokenConstraints
  , mkRaceSlotPolicy
  )
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.RacersState.Types (RacersState(RacersState))
import CardanoRacers.ScriptsFFI (raceScript)
import Contract.Address (getNetworkId)
import Contract.Chain (currentTime)
import Contract.Monad (liftContractM)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustPayToPubKey
  , mustPayToScript
  , mustReferenceOutput
  , mustSpendScriptOutput
  ) as Constraints
import Contract.Utxos (utxosAt)
import Control.Error.Util ((??))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
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

startRace :: StartRaceParams -> Racers StartRaceResult
startRace =
  startRaceWithHardcodedRewardDistribution Nothing

startRaceWithHardcodedRewardDistribution
  :: Maybe RewardDistribution -- should only be set in tests
  -> StartRaceParams
  -> Racers StartRaceResult
startRaceWithHardcodedRewardDistribution
  distribution
  (StartRaceParams startParams) = do
  slotPolicy <- mkRaceSlotPolicy startParams.raceId

  slotPolicyHash <-
    lift $ liftContractM "Could not get race slot token script hash"
      ( PlutusScript.hash <$>
          Array.head (unwrap slotPolicy).plutusMintingPolicies
      )

  (slotConstraints /\ slotLookups) <- mintRaceSlotTokenConstraints
    startParams.raceId
    [ raceStateTokenName /\ one
    , valueEscrowTokenName /\ one
    ]

  nowTime <- lift currentTime

  participantAddresses <-
    traverse
      ( \(RaceParticipant { payoutAddress }) ->
          case (unwrap payoutAddress).addressCredential of
            Plutus.PubKeyCredential _ ->
              pure payoutAddress
            _ ->
              throwError $ error
                $ "All race participants must have pkh addresses. But found: "
                <> show payoutAddress
      )
      startParams.participants

  let
    raceParams :: RaceParams
    raceParams = wrap
      { stateCurrencySymbol: slotPolicyHash
      , totalRewardValue: Plutus.Value.fromCardano startParams.totalRewardValue
      , participants: participantAddresses
      , delegates: startParams.delegates
      , escrowTtl: nowTime + mkPosixTimeUnsafe (Days 2.0)
      , feePerDelegate: Plutus.Value.fromCardano <$> startParams.feePerDelegate
      , rewardWeights: startParams.rewardWeights
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
    escrowValue = unsafePartial $ startParams.totalRewardValue <>
      valueEscrowTokenValue

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
    pure $ wrap
      { txHash
      , raceParams
      }

distributeRewards :: RaceParams -> Racers TransactionHash
distributeRewards =
  either (throwError <<< error <<< show) pure
    <=< distributeRewardsReturningErrors

distributeRewardsReturningErrors
  :: RaceParams
  -> Racers (Either DistributeRewardsContractError TransactionHash)
distributeRewardsReturningErrors raceParams = do
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

  raceValidatorUtxos <- lift $ Map.toUnfoldable <$> utxosAt raceValidatorAddress
  runExceptT do
    raceStateUtxo <- findRaceStateUtxo raceValidatorUtxos stateCurrencySymbol ??
      CouldNotFindRaceStateUtxo
    valueEscrowUtxo <-
      findValueEscrowUtxo raceValidatorUtxos stateCurrencySymbol ??
        CouldNotFindValueEscrowUtxo

    raceStateDatum <- decodeRaceDatum (snd raceStateUtxo) ??
      CouldNotDecodeRaceDatum
    rewardDistr <-
      case raceStateDatum of
        RaceState { distribution: Just distr } ->
          Plutus.Map.toCardano distr ?? CouldNotConvertDistribution
        RaceState { distribution: Nothing } ->
          throwError RewardDistributionNotAnnounced
        _ ->
          throwError UnexpectedRaceDatumVariant

    (rewards :: Array (Plutus.Address /\ Value)) <-
      traverse
        ( \(addr /\ plutusReward) ->
            case Plutus.Value.toCardano plutusReward of
              Just reward ->
                pure $ addr /\ reward
              Nothing ->
                throwError CouldNotConvertRewardValue
        )
        (Map.toUnfoldable rewardDistr)

    feePerDelegate <-
      traverse Plutus.Value.toCardano (unwrap raceParams).feePerDelegate ??
        CouldNotConvertFeePerDelegateValue

    let
      mkStateTokenValue :: AssetName -> Value
      mkStateTokenValue tn = Value.singleton stateCurrencySymbol tn BigNum.one

      stateTokens :: Value
      stateTokens =
        unsafePartial
          ( mkStateTokenValue raceStateTokenName
              <> mkStateTokenValue valueEscrowTokenName
          )

      raceStateLovelace :: Number
      raceStateLovelace = BigInt.toNumber $ BigNum.toBigInt $ unwrap $
        valueToCoin
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
        , maybe mempty
            ( \feeValue ->
                foldMap (flip Constraints.mustPayToPubKey feeValue <<< wrap)
                  (unwrap raceParams).delegates
            )
            feePerDelegate
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
    lift $ lift do
      txHash <- submitTxFromConstraints lookups constraints
      awaitTxConfirmed txHash
      pure txHash

findRaceStateUtxo
  :: Array (TransactionInput /\ TransactionOutput)
  -> ScriptHash
  -> Maybe (TransactionInput /\ TransactionOutput)
findRaceStateUtxo scriptUtxos stateCs =
  Array.find
    ( \(_ /\ txOut) ->
        valueOf (Asset stateCs raceStateTokenName) (unwrap txOut).amount
          == BigNum.one
    )
    scriptUtxos

findValueEscrowUtxo
  :: Array (TransactionInput /\ TransactionOutput)
  -> ScriptHash
  -> Maybe (TransactionInput /\ TransactionOutput)
findValueEscrowUtxo scriptUtxos stateCs =
  Array.find
    ( \(_ /\ txOut) ->
        valueOf (Asset stateCs valueEscrowTokenName) (unwrap txOut).amount
          == BigNum.one
    )
    scriptUtxos

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
