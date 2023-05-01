module CardanoRacers.RaceRegistry.Contract where

import Contract.Prelude

import CardanoRacers.RaceEnrollment.Contract (mkRaceEnrollmentPolicy)
import CardanoRacers.RaceRegistry.Types (RegistryParams)
import CardanoRacers.ScriptsFFI (raceEnrollmentPolicy)
import Contract.Address (scriptHashAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (toData, unitRedeemer)
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
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (Value, geq, mkTokenName, mpsSymbol)
import Contract.Value as Value
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.FoldableWithIndex (findWithIndex)
import Data.Map as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers, withContract)

collectRegistryScriptLeftovers :: RegistryParams -> Racers TransactionHash
collectRegistryScriptLeftovers rgp = do
  registryScript <- mkRaceRegistryScript rgp
  utxosAtRegistry <- lift $ utxosAt
    (scriptHashAddress (validatorHash registryScript) Nothing)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = foldMap (flip Constraints.mustSpendScriptOutput unitRedeemer)
      $ Map.keys utxosAtRegistry

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs utxosAtRegistry <> Lookups.validator
      registryScript

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

registerInRace :: TransactionInput -> RegistryParams -> Racers TransactionHash
registerInRace txi rgp = do
  registryScript <- mkRaceRegistryScript rgp
  enrollmentSymbol <-
    withContract (liftedM "Could not get race enrollment currency symbol")
      $ (mpsSymbol <<< mintingPolicyHash)
      <$> mkRaceEnrollmentPolicy txi rgp
  utxosAtRegistry <- lift $ utxosAt
    (scriptHashAddress (validatorHash registryScript) Nothing)
  slotTokenName <- lift $ liftContractM "could not slot token name" $
    (mkTokenName <=< hexToByteArray) "Slot" -- todo: extract into constant

  let
    oneSlotToken :: Value
    oneSlotToken = Value.singleton enrollmentSymbol slotTokenName one

  { index: registryTxi, value: registryTxo } <- lift
    $ liftContractM
        "Could not find UTxO with Slot tokens att registry script address"
    $ findWithIndex
        ( const
            ( unwrap >>> _.output >>> unwrap >>> _.amount >>>
                (_ `geq` oneSlotToken)
            )
        )
    $ utxosAtRegistry

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendScriptOutput registryTxi unitRedeemer

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.unspentOutputs (Map.singleton registryTxi registryTxo) <>
      Lookups.validator registryScript

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkRaceRegistryScript
  :: RegistryParams -> Racers Validator
mkRaceRegistryScript registryParams = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope raceEnrollmentPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData rp, toData registryParams ]
  pure $ Validator $ appliedScript
