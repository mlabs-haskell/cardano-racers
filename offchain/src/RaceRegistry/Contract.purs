module CardanoRacers.RaceRegistry.Contract
  ( queryRegistryUtxos
  , collectRegistryScriptLeftovers
  , mkRaceRegistryScript
  , initRace
  , supplyRegistrySlots
  , registerPositionInRace
  , confirmParticipatingAssets
  ) where

import Contract.Prelude

import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.GameAsset.Types (GameAssetType(DriverType, CarType))
import CardanoRacers.Nitro.Contract (burnNitroConstraints, mkNitroPolicy)
import CardanoRacers.RacePosition.Contract
  ( burnRacePositionTokenConstraints
  , mintRacePositionTokenConstraints
  , mkRacePositionPolicy
  )
import CardanoRacers.RacePosition.Types (RaceHash, slotTokenName)
import CardanoRacers.RaceRegistry.Types
  ( RaceParticipant
  , RegistryDatum
  , RegistryEntry(PendingSelection, AssetSelection)
  , RegistryParams
  , RegistryRedeemer(Enroll, SelectAssets, Collect)
  )
import CardanoRacers.ScriptsFFI (raceRegistryScript)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (PubKeyHash, scriptHashAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData
  ( OutputDatum(OutputDatum, NoOutputDatum)
  , fromData
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( Validator(Validator)
  , applyArgs
  , mintingPolicyHash
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumInline))
import Contract.TxConstraints as Constrainst
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value (Value, geq, mpsSymbol, scriptCurrencySymbol, valueOf)
import Contract.Value as Value
import Contract.Wallet (getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (concat, drop, filter, head, null, take) as Array
import Data.BigInt (BigInt)
import Data.FoldableWithIndex (findWithIndex)
import Data.Map (Map)
import Data.Map
  ( filter
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
import Racers (Racers, withContract)

collectRegistryScriptLeftovers
  :: RaceHash -> RegistryParams -> Racers TransactionHash
collectRegistryScriptLeftovers raceHash rgp = do
  registryScript <- mkRaceRegistryScript rgp
  utxosAtRegistry <- lift $ utxosAt
    (scriptHashAddress (validatorHash registryScript) Nothing)

  (authTxi /\ authTxo) <- withContract (liftedM "could not find any auth utxo")
    findAnyAuthUtxo

  registryValue <- foldMap (\txo -> (unwrap (unwrap txo).output).amount)
    <<< map fst
    <$> queryRegistryUtxos rgp

  (burnConstraint /\ burnLookups) <- burnRacePositionTokenConstraints raceHash $
    uncurry (valueOf registryValue) (unwrap rgp).slotAssetClass

  let
    collectRedeemer = wrap $ toData Collect

    constraints :: Constraints.TxConstraints Void Void
    constraints = burnConstraint <> Constraints.mustSpendPubKeyOutput authTxi <>
      ( foldMap (flip Constraints.mustSpendScriptOutput collectRedeemer)
          $ Map.keys utxosAtRegistry
      )

    lookups :: Lookups.ScriptLookups Void
    lookups = burnLookups
      <> Lookups.unspentOutputs (Map.insert authTxi authTxo utxosAtRegistry)
      <>
        Lookups.validator
          registryScript

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

getRegistryEntriesFromOutput
  :: TransactionOutputWithRefScript -> Maybe (Array RegistryEntry)
getRegistryEntriesFromOutput txo = case (unwrap (unwrap txo).output).datum of
  OutputDatum d -> unwrap <$> (fromData (unwrap d) :: Maybe RegistryDatum)
  NoOutputDatum -> Just []
  _ -> Nothing

filterRegistryUtxos
  :: RegistryParams
  -> (BigInt -> Array RegistryEntry -> Boolean)
  -> Racers UtxoMap
filterRegistryUtxos rgp predicate = do
  registryScript <- mkRaceRegistryScript rgp
  let registryAddr = scriptHashAddress (validatorHash registryScript) Nothing
  registryAddressUtxos <- lift $ utxosAt registryAddr
  -- filter all Txi Txo pairs that contain slot tokens and match given predicate
  pure $ Map.filter
    ( maybe false (lift2 (&&) ((_ >= one) <<< fst) (uncurry predicate)) <<<
        getSlotsAndEntries
    )
    registryAddressUtxos
  where
  getSlotsAndEntries
    :: TransactionOutputWithRefScript -> Maybe (BigInt /\ Array RegistryEntry)
  getSlotsAndEntries txo = getRegistryEntriesFromOutput txo <#>
    ( uncurry (valueOf (unwrap (unwrap txo).output).amount)
        (unwrap rgp).slotAssetClass /\ _
    )

queryRegistryUtxos
  :: RegistryParams
  -> Racers
       ( Map TransactionInput
           (TransactionOutputWithRefScript /\ Array RegistryEntry)
       )
queryRegistryUtxos rgp = do
  utxosWithEntries <- filterRegistryUtxos rgp (\_ _ -> true)
  pure $ Map.mapMaybe withRegistryEntries utxosWithEntries
  where
  withRegistryEntries
    :: TransactionOutputWithRefScript
    -> Maybe (TransactionOutputWithRefScript /\ Array RegistryEntry)
  withRegistryEntries txo = case getRegistryEntriesFromOutput txo of
    Nothing -> Nothing
    Just entries -> Just (txo /\ entries)

findUtxoWithAvailableSlotToken
  :: RegistryParams
  -> Racers (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
findUtxoWithAvailableSlotToken rgp = do
  Array.head <<< Map.toUnfoldable <$> filterRegistryUtxos rgp
    (\totalSlots entries -> totalSlots > length entries)

initRace
  :: RaceHash -> BigInt -> BigInt -> Racers (RegistryParams /\ TransactionHash)
initRace raceHash entryNitroFee totalSlots = do
  nitroPolicyHash <- mintingPolicyHash <$> mkNitroPolicy

  driverAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy DriverType
  carAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy CarType

  positionSymbol <-
    withContract (liftedM "Could not get position policy symbol")
      $ (mpsSymbol <<< mintingPolicyHash)
      <$> mkRacePositionPolicy raceHash
  (positionConstraints /\ positionLookups) <- mintRacePositionTokenConstraints
    raceHash
    totalSlots

  let
    rgp = wrap
      { slotAssetClass: (positionSymbol /\ slotTokenName)
      , nitroFee: entryNitroFee
      , driverAssetPolicyHash
      , carAssetPolicyHash
      , nitroPolicyHash
      }

  registryVHash <- validatorHash <$> mkRaceRegistryScript rgp

  let
    totalRaceSlotsValue :: Value
    totalRaceSlotsValue = Value.singleton positionSymbol slotTokenName
      totalSlots

    emptyRegistryDatum = wrap $ toData (wrap [] :: RegistryDatum)

    constraints :: Constraints.TxConstraints Void Void
    constraints = positionConstraints <> Constraints.mustPayToScript
      registryVHash
      emptyRegistryDatum
      DatumInline
      totalRaceSlotsValue

    lookups :: Lookups.ScriptLookups Void
    lookups = positionLookups

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure (rgp /\ txId)

supplyRegistrySlots
  :: RaceHash
  -> RegistryParams
  -> BigInt
  -> Racers TransactionHash
supplyRegistrySlots raceHash rgp slotCount = do
  (slotConstraints /\ slotLookups) <- mintRacePositionTokenConstraints raceHash
    slotCount
  registryVHash <- validatorHash <$> mkRaceRegistryScript rgp

  let
    totalRaceSlotsValue :: Value
    totalRaceSlotsValue = uncurry Value.singleton (unwrap rgp).slotAssetClass
      slotCount

    emptyRegistryDatum = wrap $ toData (wrap [] :: RegistryDatum)

    constraints :: Constraints.TxConstraints Void Void
    constraints = slotConstraints <> Constraints.mustPayToScript
      registryVHash
      emptyRegistryDatum
      DatumInline
      totalRaceSlotsValue

    lookups :: Lookups.ScriptLookups Void
    lookups = slotLookups

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

registerPositionInRace
  :: RegistryParams
  -> PubKeyHash
  -> Racers TransactionHash
registerPositionInRace rgp pkhToEnroll = do
  registryScript <- mkRaceRegistryScript rgp
  (slotTxi /\ slotTxo) <-
    withContract
      (liftedM "Could not find any valid UTxOs with free slot tokens") $
      findUtxoWithAvailableSlotToken rgp
  registryEntries <- lift
    $ liftContractM "Couldn't decode utxo datum registry entries"
    $ getRegistryEntriesFromOutput slotTxo
  let
    pkhsToEnroll :: Array PubKeyHash
    pkhsToEnroll = [ pkhToEnroll ]

    newRegistry :: Array RegistryEntry
    newRegistry = map PendingSelection pkhsToEnroll <> registryEntries

    previousValueAtRegistry :: Value
    previousValueAtRegistry = (unwrap (unwrap slotTxo).output).amount

    registryDatum = wrap $ toData (wrap newRegistry :: RegistryDatum)
    enrollRedeemer = wrap $ toData $ Enroll pkhsToEnroll

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput slotTxi enrollRedeemer
      <> Constraints.mustPayToScript (validatorHash registryScript)
        registryDatum
        DatumInline
        previousValueAtRegistry

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton slotTxi slotTxo)
      <> Lookups.validator registryScript

  (nitroConstraints /\ nitroLookups) <- burnNitroConstraints
    $ (unwrap rgp).nitroFee
    * length pkhsToEnroll

  lift do
    txId <- submitTxFromConstraints (nitroLookups <> lookups)
      (constraints <> nitroConstraints)
    awaitTxConfirmed txId
    pure txId

confirmParticipatingAssets
  :: RegistryParams -> PubKeyHash -> RaceParticipant -> Racers TransactionHash
confirmParticipatingAssets rgp pkh participant = do
  let selections = [ participant ]
  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos
  registryVal <- mkRaceRegistryScript rgp
  driverAssetSymbol <-
    withContract (liftedM "Could not get driver asset symbol")
      $ scriptCurrencySymbol
      <$> mkGameAssetPolicy DriverType
  carAssetSymbol <- withContract (liftedM "Could not get car asset symbol")
    $ scriptCurrencySymbol
    <$> mkGameAssetPolicy CarType
  registryUtxos <- queryRegistryUtxos rgp

  let
    txiEntries = map (\(txi /\ _ /\ entries) -> txi /\ entries) $
      Map.toUnfoldable registryUtxos

    -- | Allocate slots for the given race participants.
    -- Returns Nothing if there aren't enough slots for all participants.
    allocateSlots
      :: Array RaceParticipant
      -> Array (TransactionInput /\ Array RegistryEntry)
      -> Maybe (Array (TransactionInput /\ Array RegistryEntry))
    allocateSlots sels txisWithSlots =
      foldl folder (sels /\ []) txisWithSlots #
        ( \(selsLeft /\ finalTxis) ->
            if Array.null selsLeft then Just finalTxis else Nothing
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
          allocatedSlots = map AssetSelection $ Array.take (length slotsWithPkh)
            remainingSels
          newRemainingSels = Array.drop (length slotsWithPkh) remainingSels
          remainingSlots = Array.drop (length allocatedSlots) slotsWithPkh
        in
          if Array.null allocatedSlots then newRemainingSels /\ updatedTxis -- No slots allocated; accumulator remains unchanged.
          else newRemainingSels /\
            ( updatedTxis <>
                [ (txi /\ (allocatedSlots <> otherSlots <> remainingSlots)) ]
            ) -- Update the accumulator with the allocated slots and the remaining slots.

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
            amount = (unwrap (unwrap txo).output).amount
            driverVal = Value.singleton driverAssetSymbol tk one
            carVal = Value.singleton carAssetSymbol tk one
          in
            amount `geq` driverVal || amount `geq` carVal

    Map.unions
      <<< Array.concat
      <$> traverse findAssetForSelection selections

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      foldMap Constrainst.mustSpendPubKeyOutput (Map.keys assetUtxoMap)
        <> foldMap
          ( \(txi /\ txo /\ entries) ->
              Constraints.mustSpendScriptOutput txi
                (wrap $ toData $ SelectAssets)
                <> Constraints.mustPayToScript
                  (validatorHash registryVal)
                  (wrap $ toData (wrap entries :: RegistryDatum))
                  DatumInline
                  (unwrap (unwrap txo).output).amount
          )
          allocationsWithOutputs
        <> Constraints.mustBeSignedBy (wrap pkh)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( assetUtxoMap `Map.union` Map.unions
            ( map (\(txi /\ txo /\ _) -> Map.singleton txi txo)
                allocationsWithOutputs
            )
        )
        <> Lookups.validator registryVal

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
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData registryParams ]
  pure $ Validator $ appliedScript
