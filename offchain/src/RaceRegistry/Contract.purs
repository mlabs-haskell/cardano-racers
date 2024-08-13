module CardanoRacers.RaceRegistry.Contract
  ( queryRegistryUtxos
  , collectRegistryScriptLeftovers
  , mkRaceRegistryScript
  , initRace
  , supplyRegistrySlots
  , registerPositionInRace
  , confirmAssetSelection
  , findUtxoWithAvailableSlotToken
  , getRegistryEntriesFromOutput
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.MintingPolicyHash
  ( MintingPolicyHash(MintingPolicyHash)
  )
import Cardano.Plutus.Types.PubKeyHash (PubKeyHash)
import Cardano.Plutus.Types.Validator (Validator(Validator))
import Cardano.ToData (toData)
import Cardano.Types (Credential(ScriptHashCredential), TransactionOutput)
import Cardano.Types.Asset (Asset(Asset))
import Cardano.Types.BigInt as CTBigInt
import Cardano.Types.BigNum (BigNum)
import Cardano.Types.BigNum as BigNum
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash)
import Cardano.Types.PlutusScript as PlutusScript
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Nitro.Contract (burnNitroConstraints, mkNitroPolicy)
import CardanoRacers.RaceRegistry.Types
  ( RaceParticipant
  , RegistryDatum
  , RegistryEntry(PendingSelection, AssetSelection)
  , RegistryParams
  , RegistryRedeemer(Enroll, SelectAssets, Collect)
  )
import CardanoRacers.RaceSlot.Contract
  ( burnRaceSlotTokenConstraints
  , mintRaceSlotTokenConstraints
  , mkRaceSlotPolicy
  )
import CardanoRacers.RaceSlot.Types (RaceHash, slotTokenName)
import CardanoRacers.ScriptsFFI (raceRegistryScript)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (mkAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Value, geq, valueOf)
import Contract.Value as Value
import Contract.Wallet (getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array
  ( concat
  , drop
  , filter
  , find
  , head
  , null
  , replicate
  , take
  , zipWith
  ) as Array
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromString, toString) as BigInt
import Data.FoldableWithIndex (findWithIndex)
import Data.Map (Map)
import Data.Map
  ( filter
  , fromFoldable
  , insert
  , keys
  , lookup
  , mapMaybe
  , singleton
  , toUnfoldable
  , union
  , unions
  ) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)

collectRegistryScriptLeftovers
  :: RaceHash -> RegistryParams -> Int -> Racers TransactionHash
collectRegistryScriptLeftovers raceHash rgp outputsToCollect = do
  registryScript <- mkRaceRegistryScript rgp

  (authTxi /\ authTxo) <- withContract (liftedM "could not find any auth utxo")
    findAnyAuthUtxo

  registryUtxos <- queryRegistryUtxos rgp <#> Map.toUnfoldable >>> Array.take
    outputsToCollect

  let
    registryValueToBurn = unsafePartial foldMap
      (\(_ /\ (txo /\ _)) -> (unwrap txo).amount)
      registryUtxos

    assetName = Asset (fst (unwrap rgp).slotAssetClass)
      (snd (unwrap rgp).slotAssetClass)

  (burnConstraint /\ burnLookups) <- burnRaceSlotTokenConstraints raceHash $
    ( unsafePartial
        $ fromJust
        $ BigInt.fromString
        $ BigNum.toString
        $ valueOf assetName registryValueToBurn
    )

  let
    collectRedeemer = wrap $ toData Collect

    constraints :: Constraints.TxConstraints
    constraints = burnConstraint <> Constraints.mustSpendPubKeyOutput authTxi <>
      ( foldMap (flip Constraints.mustSpendScriptOutput collectRedeemer)
          $ map fst registryUtxos
      )

    lookups :: Lookups.ScriptLookups
    lookups = burnLookups
      <> Lookups.unspentOutputs
        ( Map.insert authTxi authTxo $ Map.fromFoldable $ map
            (\(i /\ (o /\ _)) -> i /\ o)
            registryUtxos
        )
      <>
        Lookups.validator (unwrap registryScript)

  lift $ do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

getRegistryEntriesFromOutput
  :: TransactionOutput -> Maybe (Array RegistryEntry)
getRegistryEntriesFromOutput txo = case (unwrap txo).datum of
  Just d -> case outputDatumDatum d of
    Just od -> unwrap <$> ((fromData od) :: Maybe RegistryDatum)
    Nothing -> Just []
  Nothing -> Just []

filterRegistryUtxos
  :: RegistryParams
  -> (BigInt -> Array RegistryEntry -> Boolean)
  -> Racers UtxoMap
filterRegistryUtxos rgp predicate = do
  registryScript <- mkRaceRegistryScript rgp
  registryAddr <- lift $ mkAddress
    (wrap $ ScriptHashCredential $ hash $ unwrap registryScript)
    Nothing
  registryAddressUtxos <- lift $ utxosAt registryAddr
  -- filter all Txi Txo pairs that contain slot tokens and match given predicate
  pure $ Map.filter
    ( maybe false (lift2 (&&) ((_ >= one) <<< fst) (uncurry predicate)) <<<
        getSlotsAndEntries
    )
    registryAddressUtxos
  where
  assetName = Asset (fst (unwrap rgp).slotAssetClass)
    (snd (unwrap rgp).slotAssetClass)

  getSlotsAndEntries
    :: TransactionOutput -> Maybe (BigInt /\ Array RegistryEntry)
  getSlotsAndEntries txo = getRegistryEntriesFromOutput txo <#>
    ( \(arrRegEntry :: Array RegistryEntry) ->
        let
          val = valueOf assetName (unwrap txo).amount
        in
          (unsafePartial $ fromJust $ BigInt.fromString $ BigNum.toString val)
            /\ arrRegEntry
    )

queryRegistryUtxos
  :: RegistryParams
  -> Racers
       ( Map TransactionInput
           (TransactionOutput /\ Array RegistryEntry)
       )
queryRegistryUtxos rgp = do
  utxosWithEntries <- filterRegistryUtxos rgp (\_ _ -> true)
  pure $ Map.mapMaybe withRegistryEntries utxosWithEntries
  where
  withRegistryEntries
    :: TransactionOutput
    -> Maybe (TransactionOutput /\ Array RegistryEntry)
  withRegistryEntries txo = case getRegistryEntriesFromOutput txo of
    Nothing -> Nothing
    Just entries -> Just (txo /\ entries)

findUtxoWithAvailableSlotToken
  :: RegistryParams
  -> Racers (Maybe (TransactionInput /\ TransactionOutput))
findUtxoWithAvailableSlotToken rgp =
  Array.head <<< Map.toUnfoldable <$> filterRegistryUtxos rgp
    (\totalSlots entries -> totalSlots > length entries)

findSlotUtxoByTxInput
  :: RegistryParams
  -> TransactionInput
  -> Racers (Maybe (TransactionInput /\ TransactionOutput))
findSlotUtxoByTxInput rgp txInput = do
  queryRegistryUtxos rgp
    <#> Map.toUnfoldable
    >>> map (\(txi /\ (txo /\ _)) -> txi /\ txo)
    >>> Array.find (\(txi /\ _) -> txInput == txi)

initRace
  :: RaceHash
  -> BigInt
  -> Int
  -> Int
  -> Racers (RegistryParams /\ TransactionHash)
initRace raceHash entryNitroFee totalSlots utxoCount = do
  nitroPolicy <- mkNitroPolicy
  nitroPolicyHash <- lift $ liftContractM "Could not get nitro script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)

  driverAssetPolicy <- mkGameAssetPolicy DriverType
  driverAssetPolicyHash <- lift
    $ liftContractM "Could not get driver asset script hash"
    $ hash
    <$> (head (unwrap driverAssetPolicy).plutusMintingPolicies)

  carAssetPolicy <- mkGameAssetPolicy CarType
  carAssetPolicyHash <- lift
    $ liftContractM "Could not get car asset script hash"
    $ hash
    <$> (head (unwrap carAssetPolicy).plutusMintingPolicies)

  slotPolicy <- mkRaceSlotPolicy raceHash

  slotPolicyHash <- lift
    $ liftContractM "Could not get race slot token script hash"
    $ head
    $ map PlutusScript.hash
    $ (unwrap slotPolicy).plutusMintingPolicies

  (slotConstraints /\ slotLookups) <- mintRaceSlotTokenConstraints
    raceHash
    (BigInt.fromInt totalSlots)

  (authTxi /\ authTxo) <- withContract (liftedM "could not find any auth utxo")
    findAnyAuthUtxo

  let
    rgp = wrap
      { slotAssetClass: (slotPolicyHash /\ (unwrap slotTokenName))
      , nitroFee: unsafePartial
          $ fromJust
          $ CTBigInt.fromString
          $ BigInt.toString entryNitroFee
      , driverAssetPolicyHash: MintingPolicyHash driverAssetPolicyHash
      , carAssetPolicyHash: MintingPolicyHash carAssetPolicyHash
      , nitroPolicyHash: MintingPolicyHash nitroPolicyHash
      }

  registryVHash <- (hash <<< unwrap) <$> mkRaceRegistryScript rgp

  let
    baseSlotPerUtxo = totalSlots `div` utxoCount
    remainderSlots = totalSlots `mod` utxoCount

    slotValue :: BigNum -> Value
    slotValue i = Value.singleton slotPolicyHash (unwrap slotTokenName) i

    emptyRegistryDatum = toData (wrap [] :: RegistryDatum)

    registryOutput :: Value -> Constraints.TxConstraints
    registryOutput v = Constraints.mustPayToScript
      registryVHash
      emptyRegistryDatum
      DatumInline
      v

    utxoVals :: Array Value
    utxoVals =
      let
        base = Array.replicate utxoCount $ slotValue $ BigNum.fromInt
          baseSlotPerUtxo
        rems = Array.replicate remainderSlots (slotValue $ BigNum.fromInt 1) <>
          Array.replicate (utxoCount - remainderSlots) (unsafePartial mempty)

      in
        Array.zipWith (unsafePartial (<>)) base rems

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendPubKeyOutput authTxi <> slotConstraints
      <> (fold $ map registryOutput utxoVals)

    lookups :: Lookups.ScriptLookups
    lookups = slotLookups <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure (rgp /\ txId)

supplyRegistrySlots
  :: RaceHash
  -> RegistryParams
  -> Int
  -> Int
  -> Racers TransactionHash
supplyRegistrySlots raceHash rgp slotCount utxoCount = do
  (slotConstraints /\ slotLookups) <- mintRaceSlotTokenConstraints raceHash
    (BigInt.fromInt slotCount)
  registryVHash <- (hash <<< unwrap) <$> mkRaceRegistryScript rgp

  let
    baseSlotPerUtxo = slotCount `div` utxoCount
    remainderSlots = slotCount `mod` utxoCount

    slotValue :: BigNum -> Value
    slotValue i = uncurry Value.singleton (unwrap rgp).slotAssetClass i

    utxoVals :: Array Value
    utxoVals =
      let
        base = Array.replicate utxoCount $ slotValue $ BigNum.fromInt
          baseSlotPerUtxo
        rems = Array.replicate remainderSlots (slotValue $ BigNum.fromInt 1) <>
          Array.replicate (utxoCount - remainderSlots) (unsafePartial mempty)
      in
        Array.zipWith (unsafePartial (<>)) base rems

    registryOutput :: Value -> Constraints.TxConstraints
    registryOutput v = Constraints.mustPayToScript
      registryVHash
      emptyRegistryDatum
      DatumInline
      v

    emptyRegistryDatum = toData (wrap [] :: RegistryDatum)

    constraints :: Constraints.TxConstraints
    constraints = slotConstraints
      <> (fold $ map registryOutput utxoVals)

    lookups :: Lookups.ScriptLookups
    lookups = slotLookups

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

registerPositionInRace
  :: RegistryParams
  -> PubKeyHash
  -> TransactionInput
  -> Racers TransactionHash
registerPositionInRace rgp pkhToEnroll slotTxi_ = do
  registryScript <- mkRaceRegistryScript rgp
  (slotTxi /\ slotTxo) <-
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens") $
      findSlotUtxoByTxInput rgp slotTxi_
  registryEntries <- lift
    $ liftContractM "Couldn't decode utxo datum registry entries"
    $ getRegistryEntriesFromOutput slotTxo

  let
    pkhsToEnroll :: Array PubKeyHash
    pkhsToEnroll = [ pkhToEnroll ]

  -- If nitro fee is set to 0 (for freerolls) then we don't need to burn any
  -- nitro
  (nitroConstraints /\ nitroLookups) <-
    if (unwrap rgp).nitroFee > CTBigInt.fromInt 0 then burnNitroConstraints
      $
        ( unsafePartial $ fromJust
            $ BigInt.fromString
            $ CTBigInt.toString
            $ (unwrap rgp).nitroFee
            * length pkhsToEnroll
        )
    else pure $ mempty /\ mempty
  let

    newRegistry :: Array RegistryEntry
    newRegistry = map PendingSelection pkhsToEnroll <> registryEntries

    previousValueAtRegistry :: Value
    previousValueAtRegistry = (unwrap slotTxo).amount

    registryDatum = toData (wrap newRegistry :: RegistryDatum)
    enrollRedeemer = wrap $ toData $ Enroll pkhsToEnroll

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendScriptOutput slotTxi enrollRedeemer
      <> Constraints.mustPayToScript (hash $ unwrap registryScript)
        registryDatum
        DatumInline
        previousValueAtRegistry
      <> nitroConstraints

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.unspentOutputs (Map.singleton slotTxi slotTxo)
      <> Lookups.validator (unwrap registryScript)
      <> nitroLookups

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

confirmAssetSelection
  :: RegistryParams -> PubKeyHash -> RaceParticipant -> Racers TransactionHash
confirmAssetSelection rgp pkh participant = do
  let selections = [ participant ]
  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos
  registryVal <- mkRaceRegistryScript rgp

  driverAssetPolicy <- mkGameAssetPolicy DriverType
  driverAssetPolicyHash <- lift
    $ liftContractM "Could not get driver asset script hash"
    $ hash
    <$> (head (unwrap driverAssetPolicy).plutusMintingPolicies)

  carAssetPolicy <- mkGameAssetPolicy CarType
  carAssetPolicyHash <- lift
    $ liftContractM "Could not get car asset script hash"
    $ hash
    <$> (head (unwrap carAssetPolicy).plutusMintingPolicies)
  registryUtxos <- queryRegistryUtxos rgp

  let
    txiEntries = map (\(txi /\ _ /\ entries) -> txi /\ entries) $
      Map.toUnfoldable registryUtxos

    -- | Allocate slots for the given race participants.
    -- | Takes an Array of RaceParticipants and an Array of RegistryEntries.
    -- | Then it tries to modify entries where the PKH matches and outputs the
    -- | new updated Array of RegistryEntries.
    -- Returns Nothing if there aren't enough slots for all participants.
    allocateSlots
      :: Array RaceParticipant
      -> Array (TransactionInput /\ Array RegistryEntry)
      -> Maybe (Array (TransactionInput /\ Array RegistryEntry))
    allocateSlots sels txisWithSlots =
      foldl folder (sels /\ []) txisWithSlots #
        ( \(remainingSels /\ finalTxis) ->
            if Array.null remainingSels then Just finalTxis
            else Nothing -- could not allocate all selections, insufficient slots purchased
        )
      where
      folder
        :: ( Array RaceParticipant /\ Array
               (TransactionInput /\ Array RegistryEntry)
           )
        -> (TransactionInput /\ Array RegistryEntry)
        -> ( Array RaceParticipant /\ Array
               (TransactionInput /\ Array RegistryEntry)
           )
      folder (remainingSels /\ updatedTxis) (txi /\ entries) =
        let
          slotsWithPkh = Array.filter (_ == (PendingSelection pkh)) entries
          otherSlots = Array.filter (_ /= (PendingSelection pkh)) entries
          newAllocatedSlots = map AssetSelection $ Array.take
            (length slotsWithPkh)
            remainingSels
          newRemainingSels = Array.drop (length slotsWithPkh) remainingSels
          remainingSlots = Array.drop (length newAllocatedSlots) slotsWithPkh
        in
          if Array.null newAllocatedSlots
          -- No slots allocated; accumulator remains unchanged. Continue with rest of entries
          then newRemainingSels /\ updatedTxis
          else newRemainingSels /\
            ( updatedTxis <>
                [ (txi /\ (newAllocatedSlots <> otherSlots <> remainingSlots)) ]
            ) -- Update the accumulator with the allocated and remaining slots.

  allocatedTxiWithEntries <- lift
    $ liftContractM
        "Could not allocate selections, not enought slots with given pkh"
    $ allocateSlots selections txiEntries
  allocationsWithOutputs <- lift $ liftContractM "Not possible" $ traverse
    ( \(txi /\ entries) -> Map.lookup txi registryUtxos <#>
        (\(txo /\ _) -> (txi /\ txo /\ entries))
    )
    allocatedTxiWithEntries

  assetUtxoMap <- do
    let
      findAssetForSelection sel =
        traverse findUtxoWithAsset [ (unwrap sel).driver, (unwrap sel).car ]

      findUtxoWithAsset tk =
        lift
          $ liftContractM
              ("Could not find asset: " <> (show tk) <> " in wallet utxos")
          $ map (lift2 Map.singleton _.index _.value)
          $ findWithIndex hasAsset utxos
        where
        hasAsset _ txo =
          let
            amount = (unwrap txo).amount
            driverVal = Value.singleton driverAssetPolicyHash tk BigNum.one
            carVal = Value.singleton carAssetPolicyHash tk BigNum.one
          in
            amount `geq` driverVal || amount `geq` carVal

    Map.unions
      <<< Array.concat
      <$> traverse findAssetForSelection selections

  let
    constraints :: Constraints.TxConstraints
    constraints =
      foldMap Constraints.mustSpendPubKeyOutput (Map.keys assetUtxoMap)
        <> foldMap
          ( \(txi /\ txo /\ entries) ->
              Constraints.mustSpendScriptOutput txi
                (wrap $ toData $ SelectAssets)
                <> Constraints.mustPayToScript
                  (hash $ unwrap registryVal)
                  (toData (wrap entries :: RegistryDatum))
                  DatumInline
                  (unwrap txo).amount
          )
          allocationsWithOutputs
        <> Constraints.mustBeSignedBy (wrap $ unwrap pkh)

    lookups :: Lookups.ScriptLookups
    lookups =
      Lookups.unspentOutputs
        ( assetUtxoMap `Map.union` Map.unions
            ( map (\(txi /\ txo /\ _) -> Map.singleton txi txo)
                allocationsWithOutputs
            )
        )
        <> Lookups.validator (unwrap registryVal)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkRaceRegistryScript
  :: RegistryParams -> Racers Validator
mkRaceRegistryScript registryParams = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceRegistryScript
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData registryParams ]
  pure $ Validator $ appliedScript
