module CardanoRacers.RaceEnrollment.Contract where

import Contract.Prelude

import CardanoRacers.RaceEnrollment.Types
  ( EnrollmentPolicyRedeemer(MintInitialSlotTokens)
  , RaceHash
  )
import CardanoRacers.RaceRegistry.Types (RegistryParams(..))
import CardanoRacers.ScriptsFFI (raceEnrollmentPolicy)
import Contract.Monad (liftContractM)
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
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.BigInt (BigInt)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

mintRaceSlotsConstraints
  :: TransactionInput
  -> RegistryParams
  -> BigInt
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintRaceSlotsConstraints txoref rgp slotCount = do
  enrollmentPolicy <- mkRaceEnrollmentPolicy txoref rgp
  enrollmentSymbol <- lift $ liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol enrollmentPolicy

  tokenName <- lift $ liftContractM "Could not create slot token name" $
    (mkTokenName <=< hexToByteArray) "Slot" -- todo: extract into constant

  let
    amountToMint :: Value
    amountToMint = Value.singleton enrollmentSymbol tokenName slotCount

    redeemer :: Redeemer
    redeemer = Redeemer $ toData $ MintInitialSlotTokens

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustMintValueWithRedeemer redeemer amountToMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy enrollmentPolicy

  pure (constraints /\ lookups)

mintRaceSlots
  :: TransactionInput
  -> RegistryParams
  -> BigInt
  -> Racers TransactionHash
mintRaceSlots txi rgp slotCount = do
  (constraints /\ lookups) <- mintRaceSlotsConstraints txi rgp slotCount
  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkRaceEnrollmentPolicy
  :: TransactionInput -> RegistryParams -> Racers MintingPolicy
mkRaceEnrollmentPolicy txoref rgp = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceEnrollmentPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData txoref, toData rp, toData rgp ]
  pure $ PlutusMintingPolicy $ appliedScript
