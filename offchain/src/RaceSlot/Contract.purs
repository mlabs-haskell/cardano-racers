module CardanoRacers.RaceSlot.Contract where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Types (AssetName)
import Cardano.Types.Int (fromString) as Int
import Cardano.Types.Mint (Mint, singleton) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import CardanoRacers.RaceSlot.Types (RaceHash)
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
import Control.Monad.Error.Class (throwError)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (null) as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Partial.Unsafe (unsafePartial)
import Racers (Racers)

mintRaceSlotTokenConstraints
  :: RaceHash
  -> Array (AssetName /\ BigInt)
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
mintRaceSlotTokenConstraints raceHash tokens = do
  when (Array.null tokens) $ throwError $ error "Empty token array"

  slotPolicy <- mkRaceSlotPolicy raceHash

  (authTxi /\ authTxo) <-
    findAnyAuthUtxo >>=
      (lift <<< liftContractM "Could not find admin or bot utxo in wallet")

  slotPolicyHash <- lift
    $ liftContractM "Could not get race slot token script hash"
    $ head
    $ map PlutusScript.hash
    $ (unwrap slotPolicy).plutusMintingPolicies

  tokens' <- lift
    $ liftContractM "Could not convert token quantities to Cardano.Types.Int"
    $ traverse (traverse (Int.fromString <<< BigInt.toString)) tokens

  let
    amountToMint :: Mint.Mint
    amountToMint =
      unsafePartial $
        foldMap (\(tn /\ quantity) -> Mint.singleton slotPolicyHash tn quantity)
          tokens'

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendPubKeyOutput authTxi
      <> Constraints.mustMintValue amountToMint

    lookups :: Lookups.ScriptLookups
    lookups = slotPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

burnRaceSlotTokenConstraints
  :: RaceHash
  -> Array (AssetName /\ BigInt)
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
burnRaceSlotTokenConstraints raceHash tokens =
  mintRaceSlotTokenConstraints raceHash (map negate <$> tokens)

mintRaceSlotToken
  :: RaceHash
  -> Array (AssetName /\ BigInt)
  -> Racers TransactionHash
mintRaceSlotToken raceHash tokens = do
  (constraints /\ lookups) <- mintRaceSlotTokenConstraints raceHash tokens
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
