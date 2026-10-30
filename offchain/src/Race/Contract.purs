module CardanoRacers.Race.Contract
  ( DistributeRewardsDiagnostics(DistributeRewardsDiagnostics)
  , RedeemerExUnits(RedeemerExUnits)
  , distributeRewards
  , distributeRewardsReturningErrors
  , distributeRewardsWithDiagnostics
  , logDistributeRewardsDiagnostics
  , mkRaceValidator
  , startRace
  , startRaceWithHardcodedRewardDistribution
  ) where

import Contract.Prelude

import Cardano.AsCbor (encodeCbor)
import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address (Address) as Plutus
import Cardano.Plutus.Types.Credential (Credential(PubKeyCredential)) as Plutus
import Cardano.Plutus.Types.Value (fromCardano, toCardano) as Plutus.Value
import Cardano.ToData (toData)
import Cardano.Types
  ( Asset(Asset)
  , AssetName
  , Coin(Coin)
  , Credential(ScriptHashCredential)
  , ExUnits(ExUnits)
  , PlutusScript
  , Redeemer(Redeemer)
  , RedeemerDatum
  , RedeemerTag
  , ScriptHash
  , Transaction(Transaction)
  , TransactionBody(TransactionBody)
  , TransactionHash
  , TransactionInput
  , TransactionOutput(TransactionOutput)
  , TransactionWitnessSet(TransactionWitnessSet)
  , Value
  )
import Cardano.Types.Address (mkPaymentAddress)
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum
  ( fromInt
  , fromStringUnsafe
  , one
  , toBigInt
  , toString
  ) as BigNum
import Cardano.Types.MultiAsset (empty, flatten) as MultiAsset
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.ProtocolParameters
  ( ProtocolParameters(ProtocolParameters)
  )
import Cardano.Types.Value
  ( getCoin
  , getMultiAsset
  , lovelaceValueOf
  , valueOf
  , valueToCoin
  )
import Cardano.Types.Value (singleton) as Value
import CardanoRacers.Helpers (mkPosixTimeUnsafe, paysToAddrConstraint)
import CardanoRacers.Race.Types
  ( DistributeRewardsContractError
      ( CouldNotFindRaceStateUtxo
      , CouldNotFindValueEscrowUtxo
      , CouldNotDecodeRaceDatum
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
import Contract.BalanceTxConstraints
  ( BalancerConstraints
  , mustUseCollateralUtxos
  ) as BalanceTxConstraints
import Contract.Chain (currentTime)
import Contract.Log (logInfo')
import Contract.Monad (liftContractM, liftedE, liftedM)
import Contract.ProtocolParameters (getProtocolParameters)
import Contract.ScriptLookups (ScriptLookups)
import Contract.ScriptLookups (unspentOutputs, validator) as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( awaitTxConfirmed
  , createAdditionalUtxos
  , defaultBalancer
  , signTransaction
  , submit
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline), TxConstraints)
import Contract.TxConstraints
  ( mustPayToPubKey
  , mustPayToScript
  , mustReferenceOutput
  , mustSpendPubKeyOutput
  , mustSpendScriptOutput
  ) as Constraints
import Contract.UnbalancedTx (mkUnbalancedTxE)
import Contract.Utxos (utxosAt)
import Contract.Wallet (ownPaymentPubKeyHashes)
import Control.Error.Util ((??))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Logger.Class (class MonadLogger)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (find, head, length, mapWithIndex, reverse) as Array
import Data.Bifunctor (lmap)
import Data.ByteArray (byteArrayToHex, byteLength) as ByteArray
import Data.Foldable (for_, sum) as Foldable
import Data.Int (ceil, round, toNumber)
import Data.Map (filter, findMin, fromFoldable, singleton, toUnfoldable) as Map
import Data.Newtype (wrap)
import Data.Time.Duration (Days(Days))
import Data.Traversable (traverse)
import Data.Tuple (uncurry)
import Data.UInt (toInt) as UInt
import Effect.Exception (error)
import JS.BigInt (BigInt)
import JS.BigInt (fromInt, toNumber) as BigInt
import JS.BigInt (toString) as JSBigInt
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
distributeRewardsReturningErrors raceParams =
  buildDistributeRewardsTx raceParams >>= case _ of
    Left err -> pure (Left err)
    Right (lookups /\ constraints /\ balancerConstraints) -> lift do
      unbalancedTx /\ usedUtxos <- liftedE $ mkUnbalancedTxE lookups constraints
      balancedTx <- liftEither =<< defaultBalancer unbalancedTx
        { balancerConstraints, extraUtxos: usedUtxos }
      signedTx <- signTransaction balancedTx
      txHash <- submit signedTx
      awaitTxConfirmed txHash
      pure (Right txHash)

newtype RedeemerExUnits = RedeemerExUnits
  { tag :: RedeemerTag
  , index :: BigNum
  , mem :: BigNum
  , steps :: BigNum
  , memUtilizationPct :: Number
  , stepsUtilizationPct :: Number
  }

derive instance Newtype RedeemerExUnits _
derive instance Eq RedeemerExUnits
derive instance Generic RedeemerExUnits _

instance Show RedeemerExUnits where
  show = genericShow

newtype DistributeRewardsDiagnostics = DistributeRewardsDiagnostics
  { txHash :: TransactionHash
  , feeLovelace :: BigNum
  , cborSizeBytes :: Int
  , numInputs :: Int
  , numOutputs :: Int
  , numNonAdaAssetsAcrossOutputs :: Int
  , redeemers :: Array RedeemerExUnits
  , totalExUnitsMem :: BigInt
  , totalExUnitsSteps :: BigInt
  , maxTxExUnitsMem :: BigNum
  , maxTxExUnitsSteps :: BigNum
  , maxTxSizeBytes :: Int
  , memUtilizationPct :: Number
  , stepsUtilizationPct :: Number
  , sizeUtilizationPct :: Number
  }

derive instance Newtype DistributeRewardsDiagnostics _
derive instance Eq DistributeRewardsDiagnostics
derive instance Generic DistributeRewardsDiagnostics _

instance Show DistributeRewardsDiagnostics where
  show = genericShow

logDistributeRewardsDiagnostics
  :: forall m. MonadLogger m => DistributeRewardsDiagnostics -> m Unit
logDistributeRewardsDiagnostics (DistributeRewardsDiagnostics d) = do
  logInfo' "distributeRewards diagnostics:"
  logInfo' $ "  txHash                        : "
    <> ByteArray.byteArrayToHex (unwrap (encodeCbor d.txHash))
  logInfo' $ "  fee (lovelace)                : "
    <> BigNum.toString d.feeLovelace
  logInfo' $ "  cbor size (bytes)             : " <> show d.cborSizeBytes
    <> " / "
    <> show d.maxTxSizeBytes
    <> " ("
    <> fmtPct d.sizeUtilizationPct
    <> ")"
  logInfo' $ "  inputs                        : " <> show d.numInputs
  logInfo' $ "  outputs                       : " <> show d.numOutputs
  logInfo' $ "  non-ADA assets across outputs : "
    <> show d.numNonAdaAssetsAcrossOutputs
  logInfo' $ "  total ex units mem            : "
    <> JSBigInt.toString d.totalExUnitsMem
    <> " / "
    <> BigNum.toString d.maxTxExUnitsMem
    <> " ("
    <> fmtPct d.memUtilizationPct
    <> ")"
  logInfo' $ "  total ex units steps          : "
    <> JSBigInt.toString d.totalExUnitsSteps
    <> " / "
    <> BigNum.toString d.maxTxExUnitsSteps
    <> " ("
    <> fmtPct d.stepsUtilizationPct
    <> ")"
  logInfo' "  per-redeemer ex units:"
  Foldable.for_ d.redeemers \(RedeemerExUnits r) ->
    logInfo' $ "    " <> show r.tag <> "[" <> BigNum.toString r.index
      <> "] mem="
      <> BigNum.toString r.mem
      <> " ("
      <> fmtPct r.memUtilizationPct
      <> ")"
      <> " steps="
      <> BigNum.toString r.steps
      <> " ("
      <> fmtPct r.stepsUtilizationPct
      <> ")"
  where
  fmtPct :: Number -> String
  fmtPct n = show (toNumber (round (n * 100.0)) / 100.0) <> "%"

distributeRewardsWithDiagnostics
  :: RaceParams
  -> Racers
       (Either DistributeRewardsContractError DistributeRewardsDiagnostics)
distributeRewardsWithDiagnostics raceParams =
  buildDistributeRewardsTx raceParams >>= case _ of
    Left err -> pure (Left err)
    Right (lookups /\ constraints /\ balancerConstraints) -> lift do
      unbalancedTx /\ usedUtxos <- liftedE $ mkUnbalancedTxE lookups constraints
      balancedTx <- liftEither =<< defaultBalancer unbalancedTx
        { balancerConstraints, extraUtxos: usedUtxos }
      signedTx <- signTransaction balancedTx
      txHash <- submit signedTx
      awaitTxConfirmed txHash
      pparams <- getProtocolParameters
      pure (Right (mkDistributeRewardsDiagnostics pparams txHash signedTx))

-- Mainnet protocol-parameter limits hardcoded here so diagnostics reflect
-- production headroom even when run against a local testnet with inflated
-- budgets.
mainnetMaxTxExUnitsMem :: BigNum
mainnetMaxTxExUnitsMem = BigNum.fromStringUnsafe "16500000"

mainnetMaxTxExUnitsSteps :: BigNum
mainnetMaxTxExUnitsSteps = BigNum.fromStringUnsafe "10000000000"

mkDistributeRewardsDiagnostics
  :: ProtocolParameters
  -> TransactionHash
  -> Transaction
  -> DistributeRewardsDiagnostics
mkDistributeRewardsDiagnostics (ProtocolParameters pparams) txHash tx =
  let
    Transaction { body: txBody, witnessSet: txWits } = tx
    TransactionBody { inputs, outputs, fee } = txBody
    TransactionWitnessSet { redeemers } = txWits
    Coin feeLovelace = fee
    maxTxExUnitsMem = mainnetMaxTxExUnitsMem
    maxTxExUnitsSteps = mainnetMaxTxExUnitsSteps
    maxTxSizeBytes = UInt.toInt pparams.maxTxSize
    cborSizeBytes = ByteArray.byteLength $ unwrap $ encodeCbor tx
    redeemerExUnits = redeemers <#> \(Redeemer r) ->
      let
        ExUnits eu = r.exUnits
      in
        RedeemerExUnits
          { tag: r.tag
          , index: r.index
          , mem: eu.mem
          , steps: eu.steps
          , memUtilizationPct:
              pctBigIntOverBigNum (BigNum.toBigInt eu.mem) maxTxExUnitsMem
          , stepsUtilizationPct:
              pctBigIntOverBigNum (BigNum.toBigInt eu.steps) maxTxExUnitsSteps
          }
    totalExUnitsMem =
      Foldable.sum $ redeemerExUnits
        <#> BigNum.toBigInt
        <<< _.mem
        <<< unwrap
    totalExUnitsSteps =
      Foldable.sum $ redeemerExUnits
        <#> BigNum.toBigInt
        <<< _.steps
        <<< unwrap
  in
    DistributeRewardsDiagnostics
      { txHash
      , feeLovelace
      , cborSizeBytes
      , numInputs: Array.length inputs
      , numOutputs: Array.length outputs
      , numNonAdaAssetsAcrossOutputs:
          Foldable.sum $ outputs <#>
            \(TransactionOutput { amount }) ->
              Array.length (MultiAsset.flatten (getMultiAsset amount))
      , redeemers: redeemerExUnits
      , totalExUnitsMem
      , totalExUnitsSteps
      , maxTxExUnitsMem
      , maxTxExUnitsSteps
      , maxTxSizeBytes
      , memUtilizationPct: pctBigIntOverBigNum totalExUnitsMem maxTxExUnitsMem
      , stepsUtilizationPct:
          pctBigIntOverBigNum totalExUnitsSteps maxTxExUnitsSteps
      , sizeUtilizationPct: pctIntOverInt cborSizeBytes maxTxSizeBytes
      }
  where
  pctBigIntOverBigNum :: BigInt -> BigNum -> Number
  pctBigIntOverBigNum used limit =
    let
      limitN = BigInt.toNumber (BigNum.toBigInt limit)
    in
      if limitN == 0.0 then 0.0
      else BigInt.toNumber used * 100.0 / limitN

  pctIntOverInt :: Int -> Int -> Number
  pctIntOverInt used limit =
    if limit == 0 then 0.0
    else toNumber used * 100.0 / toNumber limit

prepareDistributeRewardsInput
  :: Racers (TransactionInput /\ TransactionOutput)
prepareDistributeRewardsInput = do
  ownPkh <- lift $ liftedM "could not get own payment pub key hash"
    (Array.head <$> ownPaymentPubKeyHashes)
  let
    prepLovelace = BigNum.fromStringUnsafe "10000000"
    constraints = Constraints.mustPayToPubKey ownPkh
      (lovelaceValueOf prepLovelace)
    lookups = mempty :: ScriptLookups
  lift do
    unbalancedTx /\ usedUtxos <- liftedE $ mkUnbalancedTxE lookups constraints
    balancedTx <- liftEither =<< defaultBalancer unbalancedTx
      { balancerConstraints: mempty
      , extraUtxos: usedUtxos
      }
    signedTx <- signTransaction balancedTx
    createdUtxos <- createAdditionalUtxos signedTx
    txHash <- submit signedTx
    awaitTxConfirmed txHash
    let
      isCleanPrepOutput (TransactionOutput txo) =
        getMultiAsset txo.amount == MultiAsset.empty
          && unwrap (getCoin txo.amount)
          == prepLovelace
    case Map.findMin (Map.filter isCleanPrepOutput createdUtxos) of
      Just { key: txi, value: txo } -> pure (txi /\ txo)
      Nothing -> throwError $ error
        "prepareDistributeRewardsInput: prep tx did not create expected ADA-only output"

buildDistributeRewardsTx
  :: RaceParams
  -> Racers
       ( Either DistributeRewardsContractError
           ( ScriptLookups
               /\ TxConstraints
               /\ BalanceTxConstraints.BalancerConstraints
           )
       )
buildDistributeRewardsTx raceParams = do
  prepTxi /\ prepTxo <- prepareDistributeRewardsInput
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
    rewardDistrPairs <-
      case raceStateDatum of
        -- Iterate the raw Plutus.Map (Array of pairs) directly. Going via
        -- `Plutus.Map.toCardano` would reorder entries by key, which would
        -- break alignment with the on-chain `PlutusTx.AssocMap.toList`
        -- (which returns the datum's insertion order).
        RaceState { distribution: Just distr } ->
          pure (unwrap distr)
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
        rewardDistrPairs

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

      -- Output indices passed to the on-chain validator so indexed checks
      -- do O(1) lookups instead of scanning all outputs.
      --
      -- CTL's `ProcessConstraints` prepends each `mustPay*` output via
      -- `Array.(:)`, and the balancer appends change at the end. So the
      -- LAST constraint-emitted output lands at index 0, and the first at
      -- the highest constraint-emitted index.
      --
      -- `rewards` is reversed before `foldMap` so the processing order
      -- within the rewards group lines up with the final index order,
      -- giving `rewardOutputIxs[i] = 3 + numFees + i`. Final positions:
      --   0: operating
      --   1: treasury
      --   2: TokenBin
      --   3 .. 2+d: fees (fee[d-1] at 3, fee[0] at 2+d)
      --   3+d .. 2+d+k: rewards (reward[0] at 3+d, reward[k-1] at 2+d+k)
      numFees = case feePerDelegate of
        Just _ -> Array.length (unwrap raceParams).delegates
        Nothing -> 0
      operatingOutputIx = BigInt.fromInt 0
      treasuryOutputIx = BigInt.fromInt 1
      rewardOutputIxs = Array.mapWithIndex
        (\i _ -> BigInt.fromInt (3 + numFees + i))
        rewards

      redeemer :: RedeemerDatum
      redeemer = wrap $ toData $ DistributeRewards
        { treasuryOutputIx, operatingOutputIx, rewardOutputIxs }

      constraints :: TxConstraints
      constraints = mconcat
        [ Constraints.mustSpendScriptOutput (fst raceStateUtxo) redeemer
        , Constraints.mustSpendScriptOutput (fst valueEscrowUtxo) redeemer
        , Constraints.mustSpendPubKeyOutput prepTxi
        , foldMap (uncurry paysToAddrConstraint) (Array.reverse rewards)
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
            , prepTxi /\ prepTxo
            ]
        , Lookups.validator raceValidator
        ]

      balancerConstraints :: BalanceTxConstraints.BalancerConstraints
      balancerConstraints = BalanceTxConstraints.mustUseCollateralUtxos
        (Map.singleton prepTxi prepTxo)
    pure (lookups /\ constraints /\ balancerConstraints)

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
