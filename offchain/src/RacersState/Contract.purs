module CardanoRacers.RacersState.Contract where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.RacersState.Types
  ( RacersState
  , RacersStateRedeemer(SetRacersState)
  )
import CardanoRacers.ScriptsFFI (racersStateValidatorScript)
import Contract.Address (scriptHashAddress)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts (Validator(Validator), applyArgs, validatorHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (geq)
import Contract.Value (singleton) as Value
import Data.Array (singleton) as Array
import Data.Map (singleton, toUnfoldable, union) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

-- | Given RacersParams attempts to lock the StateToken with an inline
-- | RacersState datum at validator script
-- | throws InsufficientTxInputs if state token is not in current wallets
-- | balance
initRacersStateContract
  :: RacersParams -> RacersState -> Contract TransactionHash
initRacersStateContract np ns = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  racersVal <- mkRacersStateValidator np
  let
    datum = Datum $ toData ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript (validatorHash racersVal) datum
      Constraints.DatumInline
      stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator racersVal <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given RacersParams and a state attempts to consume current state UTxO
-- | and create a new UTxO with the new state.
-- | throws if admin token is not present in wallet balance or if state token is
-- | not already locked at script
modifyRacersStateContract
  :: RacersParams -> RacersState -> Contract TransactionHash
modifyRacersStateContract np rs = do
  racersVal <- mkRacersStateValidator np
  let
    vhash = validatorHash racersVal
    datum = Datum $ toData $ rs
    red = Redeemer $ toData $ SetRacersState $ rs
    stateVal = uncurry Value.singleton (unwrap np).stateToken one
    adminVal = uncurry Value.singleton (unwrap np).adminToken one

  ownUtxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  (_ /\ stateTxi /\ stateTxo) <- queryRacersState np
  (adminTxi /\ _) <- liftContractM "Could not find admin token in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable ownUtxos :: Array _)

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput adminTxi
      <> Constraints.mustSpendScriptOutput stateTxi red
      <> Constraints.mustPayToScript vhash datum Constraints.DatumInline
        stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator racersVal
      <> Lookups.unspentOutputs
        (Map.union ownUtxos (Map.singleton stateTxi stateTxo))

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given parameters attempts to get current onchain state/prices
queryRacersState
  :: RacersParams
  -> Contract
       (RacersState /\ TransactionInput /\ TransactionOutputWithRefScript)
queryRacersState nsp = do
  vhash <- validatorHash <$> mkRacersStateValidator nsp
  let
    stateAssetClass = (unwrap nsp).stateToken
    scriptAddress = scriptHashAddress vhash Nothing
    stateVal = uncurry Value.singleton stateAssetClass one
  scriptUtxos <- utxosAt scriptAddress
  stateTxi /\ stateTxo <-
    liftContractM "Could not find utxos with state token"
      $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` stateVal)
      $ (Map.toUnfoldable scriptUtxos :: Array _)

  dat <-
    liftContractM "State UTxO does not contain datum or datum is not inline" $
      case (unwrap (unwrap stateTxo).output).datum of
        OutputDatum d -> Just d
        _ -> Nothing
  ns <- liftContractM "Could not deserialise into RacersState" $ fromData $
    unwrap
      dat
  pure $ ns /\ stateTxi /\ stateTxo

mkRacersStateValidator :: RacersParams -> Contract Validator
mkRacersStateValidator params = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope racersStateValidatorScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData params
  pure $ Validator $ appliedScript
