module CardanoRacers.RacePosition.Contract where

import Contract.Prelude

import CardanoRacers.RacePosition.Types (RaceHash, slotTokenName)
import CardanoRacers.ScriptsFFI (racePositionPolicy)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Monad (liftContractM)
import Contract.PlutusData (toData, unitRedeemer)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
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

mintRacePositionTokenConstraints
  :: RaceHash
  -> BigInt
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintRacePositionTokenConstraints rch slotCount = do
  positionPolicy <- mkRacePositionPolicy rch
  positionSymbol <- lift $ liftContractM "Could not get currency symbol" $
    Value.scriptCurrencySymbol positionPolicy

  (authTxi /\ authTxo) <-
    findAnyAuthUtxo >>=
      (lift <<< liftContractM "could not find admin or bot utxo in wallet")

  let
    amountToMint :: Value
    amountToMint = Value.singleton positionSymbol slotTokenName slotCount

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput authTxi
      <> Constraints.mustMintValueWithRedeemer unitRedeemer amountToMint

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy positionPolicy <> Lookups.unspentOutputs
      (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

mintRacePositionToken
  :: RaceHash
  -> BigInt
  -> Racers TransactionHash
mintRacePositionToken rch slotCount = do
  (constraints /\ lookups) <- mintRacePositionTokenConstraints rch slotCount
  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkRacePositionPolicy
  :: RaceHash -> Racers MintingPolicy
mkRacePositionPolicy rch = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope racePositionPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData rch ]
  pure $ PlutusMintingPolicy $ appliedScript
