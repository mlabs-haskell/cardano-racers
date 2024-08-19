module CardanoRacers.RaceSlot.Contract where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Types.Int (fromString) as Int
import Cardano.Types.Mint (Mint, singleton) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import CardanoRacers.RaceSlot.Types (RaceHash, slotTokenName)
import CardanoRacers.ScriptsFFI (raceSlotPolicy)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Monad (liftContractM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups, plutusMintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

mintRaceSlotTokenConstraints
  :: RaceHash
  -> BigInt
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
mintRaceSlotTokenConstraints rch slotCount = do
  slotPolicy <- mkRaceSlotPolicy rch

  (authTxi /\ authTxo) <-
    findAnyAuthUtxo >>=
      (lift <<< liftContractM "could not find admin or bot utxo in wallet")

  slotCountI <- lift
    $ liftContractM "Could not convert slotCount to integer"
    $ Int.fromString
    $ BigInt.toString slotCount

  slotPolicyHash <- lift
    $ liftContractM "Could not get race slot token script hash"
    $ head
    $ map PlutusScript.hash
    $ (unwrap slotPolicy).plutusMintingPolicies

  let
    amountToMint :: Mint.Mint
    amountToMint = Mint.singleton slotPolicyHash (unwrap slotTokenName)
      slotCountI

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendPubKeyOutput authTxi
      <> Constraints.mustMintValue amountToMint

    lookups :: Lookups.ScriptLookups
    lookups = slotPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

burnRaceSlotTokenConstraints
  :: RaceHash
  -> BigInt
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
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
  :: RaceHash -> Racers ScriptLookups
mkRaceSlotPolicy rch = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceSlotPolicy
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData rch ]
  pure $ plutusMintingPolicy $ appliedScript
