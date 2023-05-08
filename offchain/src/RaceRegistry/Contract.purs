module CardanoRacers.RaceRegistry.Contract where

import Contract.Prelude

import CardanoRacers.Common.Types (nitroToken)
import CardanoRacers.GameAsset.Contract (mkGameAssetPolicy)
import CardanoRacers.Nitro.Contract (burnNitroConstraints, mkNitroPolicy)
import CardanoRacers.RacePosition.Contract
  ( mintRacePositionTokenConstraints
  , mkRacePositionPolicy
  )
import CardanoRacers.RacePosition.Types (RaceHash, slotTokenName)
import CardanoRacers.RaceRegistry.Types
  ( RaceParticipant(..)
  , RegistryDatum(..)
  , RegistryEntry(..)
  , RegistryParams(..)
  , RegistryRedeemer(..)
  )
import CardanoRacers.ScriptsFFI (raceRegistryScript)
import Contract.Address (PubKeyHash(..), scriptHashAddress)
import Contract.AssocMap (mapMaybe)
import Contract.Monad (liftContractM, liftedM, withContractEnv)
import Contract.PlutusData
  ( Datum(..)
  , OutputDatum(..)
  , Redeemer(..)
  , fromData
  , toData
  , unitDatum
  , unitRedeemer
  )
import Contract.Prim.ByteArray (hexToByteArray)
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
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constrainst
import Contract.TxConstraints as Constraints
import Contract.Utxos (UtxoMap, utxosAt)
import Contract.Value
  ( Value
  , geq
  , mkTokenName
  , mpsSymbol
  , scriptCurrencySymbol
  , valueOf
  )
import Contract.Value as Value
import Contract.Wallet (getWalletUtxos)
import Control.Apply (lift2)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Ctl.Internal.Contract.Wallet (ownPubKeyHashes)
import Data.Array (cons, drop, filter, head, null, take, uncons) as Array
import Data.Array (partition)
import Data.BigInt (BigInt)
import Data.Bitraversable (rtraverse)
import Data.FoldableWithIndex (findWithIndex)
import Data.Map (Map)
import Data.Map
  ( filter
  , keys
  , lookup
  , mapMaybe
  , singleton
  , toUnfoldable
  , union
  , unions
  , values
  ) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers, withContract)

collectRegistryScriptLeftovers :: RegistryParams -> Racers TransactionHash
collectRegistryScriptLeftovers rgp = do
  registryScript <- mkRaceRegistryScript rgp
  utxosAtRegistry <- lift $ utxosAt
    (scriptHashAddress (validatorHash registryScript) Nothing)

  let
    collectRedeemer = Redeemer $ toData Collect

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      foldMap (flip Constraints.mustSpendScriptOutput collectRedeemer)
        $ Map.keys utxosAtRegistry

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs utxosAtRegistry <> Lookups.validator
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
  gameAssetPolicyHash <- mintingPolicyHash <$> mkGameAssetPolicy
  positionSymbol <-
    withContract (liftedM "Could not get position policy symbol")
      $ (mpsSymbol <<< mintingPolicyHash)
      <$> mkRacePositionPolicy raceHash
  (positionConstraints /\ positionLookups) <- mintRacePositionTokenConstraints
    raceHash
    totalSlots

  let
    rgp = RegistryParams
      { slotAssetClass: (positionSymbol /\ slotTokenName)
      , nitroFee: entryNitroFee
      , gameAssetPolicyHash
      , nitroPolicyHash
      }

  registryVHash <- validatorHash <$> mkRaceRegistryScript rgp

  let
    totalRaceSlotsValue :: Value
    totalRaceSlotsValue = Value.singleton positionSymbol slotTokenName
      totalSlots

    emptyRegistryDatum = Datum $ toData (wrap [] :: RegistryDatum)

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

registerPositionInRace
  :: RegistryParams
  -> PubKeyHash
  -> Racers (TransactionHash /\ TransactionInput)
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

    registryDatum = Datum $ toData (wrap newRegistry :: RegistryDatum)
    enrollRedeemer = Redeemer $ toData $ Enroll pkhsToEnroll

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
    pure (txId /\ slotTxi)

confirmParticipatingAssets
  :: RegistryParams -> PubKeyHash -> RaceParticipant -> Racers TransactionHash
confirmParticipatingAssets rgp pkh participant = do
  let selections = [ participant ]
  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos
  registryVal <- mkRaceRegistryScript rgp
  gameAssetSymbol <- withContract (liftedM "Could not get game asset symbol")
    $ scriptCurrencySymbol
    <$> mkGameAssetPolicy

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

  let
    assetUtxos = Map.unions $ map
      ( \selection ->
          Map.filter
            ( \txo -> lift2 (||)
                ( _ `geq` Value.singleton gameAssetSymbol
                    (unwrap selection).driver
                    one
                )
                ( _ `geq` Value.singleton gameAssetSymbol (unwrap selection).car
                    one
                )
                (unwrap (unwrap txo).output).amount
            )
            utxos
      )
      selections

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      foldMap Constrainst.mustSpendPubKeyOutput (Map.keys assetUtxos)
        <> foldMap
          ( \(txi /\ txo /\ entries) ->
              Constraints.mustSpendScriptOutput txi
                (Redeemer $ toData $ SelectAssets)
                <> Constraints.mustPayToScript
                  (validatorHash registryVal)
                  (Datum $ toData (wrap entries :: RegistryDatum))
                  DatumInline
                  (unwrap (unwrap txo).output).amount
          )
          allocationsWithOutputs
        <> Constraints.mustBeSignedBy (wrap pkh)

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.unspentOutputs
        ( assetUtxos `Map.union` Map.unions
            ( map (\(txi /\ txo /\ _) -> Map.singleton txi txo)
                allocationsWithOutputs
            )
        ) -- probably not necessary to include full registryUtxos, not sure it affects Tx size

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
