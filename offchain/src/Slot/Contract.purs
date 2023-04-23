module CardanoRacers.Slot.Contract where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.ScriptsFFI (slotTokenPolicy)
import CardanoRacers.Slot.Types (RaceHash, SlotTokenPolicyRedeemer(..))
import Contract.Monad (Contract, liftContractM)
import Contract.PlutusData (Redeemer(..), toData)
import Contract.Prim.ByteArray (hexToByteArray)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (Value, mkTokenName)
import Contract.Value as Value
import Data.BigInt (BigInt)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

mintRaceSlotsConstraints
  :: TransactionInput
  -> RacersParams
  -> RaceHash
  -> BigInt
  -> Contract
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintRaceSlotsConstraints txoref rp raceHash slotCount = do
  slotTokenPolicy <- mkSlotTokenPolicy txoref rp raceHash
  slotTokenSymbol <- liftContractM "could not get currency symbol" $
    Value.scriptCurrencySymbol slotTokenPolicy
  tokenName <- liftContractM "could not create token name from race hash" $
    (mkTokenName <=< hexToByteArray) raceHash

  let
    amountToMint :: Value
    amountToMint = Value.singleton slotTokenSymbol tokenName slotCount

    redeemer :: Redeemer
    redeemer = Redeemer $ toData $ MintSlotToken slotCount

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValueWithRedeemer redeemer amountToMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy slotTokenPolicy

  pure (constraints /\ lookups)

mintRaceSlots
  :: TransactionInput
  -> RacersParams
  -> RaceHash
  -> BigInt
  -> Contract TransactionHash
mintRaceSlots txi rp raceHash slotCount = do
  (constraints /\ lookups) <- mintRaceSlotsConstraints txi rp raceHash slotCount
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

mkSlotTokenPolicy
  :: TransactionInput -> RacersParams -> RaceHash -> Contract MintingPolicy
mkSlotTokenPolicy txoref rp raceHash = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope slotTokenPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData txoref, toData rp, toData raceHash ]
  pure $ PlutusMintingPolicy $ appliedScript
