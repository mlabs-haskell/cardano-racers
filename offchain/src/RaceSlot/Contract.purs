module CardanoRacers.RaceSlot.Contract where

import Contract.Prelude

import CardanoRacers.RaceSlot.Types (RaceHash, slotTokenName)
import CardanoRacers.ScriptsFFI (raceSlotPolicy)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Monad (liftContractM)
import Contract.PlutusData (toData, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (Value)
import Contract.Value as Value
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.BigInt (BigInt)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

mintRaceSlotTokenConstraints
  :: RaceHash
  -> BigInt
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintRaceSlotTokenConstraints rch slotCount = do
  slotPolicy <- mkRaceSlotPolicy rch
  slotSymbol <- lift $ liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol slotPolicy

  (authTxi /\ authTxo) <-
    findAnyAuthUtxo >>=
      (lift <<< liftContractM "could not find admin or bot utxo in wallet")

  let
    amountToMint :: Value
    amountToMint = Value.singleton slotSymbol slotTokenName slotCount

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput authTxi
      <> Constraints.mustMintValueWithRedeemer unitRedeemer amountToMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy slotPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

burnRaceSlotTokenConstraints
  :: RaceHash
  -> BigInt
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
burnRaceSlotTokenConstraints rch slotCount =
  mintRaceSlotTokenConstraints rch (negate slotCount)

mintRaceSlotToken
  :: RaceHash
  -> BigInt
  -> Racers TransactionHash
mintRaceSlotToken rch slotCount = do
  (constraints /\ lookups) <- mintRaceSlotTokenConstraints rch slotCount
  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkRaceSlotPolicy
  :: RaceHash -> Racers MintingPolicy
mkRaceSlotPolicy rch = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceSlotPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData rch ]
  pure $ PlutusMintingPolicy $ appliedScript
