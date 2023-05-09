module CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , modifyRacersStateContract
  , queryRacersState
  , createRacersRefScriptOutput
  , queryRacersRefScriptOutput
  , mkRacersStateValidator
  ) where

import Contract.Prelude

import CardanoRacers.RacersState.Types
  ( RacersState
  , RacersStateRedeemer(SetRacersState)
  )
import CardanoRacers.ScriptsFFI (racersStateValidatorScript)
import Common.ContractHelpers (findAdminAuthUtxo)
import Contract.Address (scriptHashAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , PlutusData
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( PlutusScript
  , ScriptHash
  , Validator(Validator)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( ScriptRef(PlutusScriptRef)
  , TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (utxosAt)
import Contract.Value (geq)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Wallet (getWalletUtxos)
import Control.Monad.Reader.Class (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton) as Array
import Data.BigInt as BigInt
import Data.FoldableWithIndex (findWithIndex)
import Data.Map (singleton, toUnfoldable, union) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

-- | Given RacersParams attempts to lock the StateToken with an inline
-- | RacersState datum at validator script
-- | throws InsufficientTxInputs if state token is not in current wallets
-- | balance
initRacersStateContract
  :: RacersState -> Racers TransactionHash
initRacersStateContract ns = do
  utxos <- lift $ liftedM "Could not get wallet utxos" getWalletUtxos
  racersVal <- mkRacersStateValidator
  rp <- asks _.params
  let
    datum = Datum $ toData ns
    stateVal = uncurry Value.singleton (unwrap rp).stateToken one

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript (validatorHash racersVal) datum
      Constraints.DatumInline
      stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator racersVal <> Lookups.unspentOutputs utxos

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

-- | Given RacersParams and a state attempts to consume current state UTxO
-- | and create a new UTxO with the new state.
-- | throws if admin token is not present in wallet balance or if state token is
-- | not already locked at script
modifyRacersStateContract
  :: RacersState -> Racers TransactionHash
modifyRacersStateContract rs = do
  racersVal <- mkRacersStateValidator
  rp <- asks _.params
  let
    vhash = validatorHash racersVal
    datum = Datum $ toData $ rs
    red = Redeemer $ toData $ SetRacersState $ rs
    stateVal = uncurry Value.singleton (unwrap rp).stateToken one

  (_ /\ stateTxi /\ stateTxo) <- queryRacersState
  (adminTxi /\ adminTxo) <- findAdminAuthUtxo >>=
    (lift <<< liftContractM "Could not find admin token in wallet")

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput adminTxi
      <> Constraints.mustSpendScriptOutput stateTxi red
      <> Constraints.mustPayToScript vhash datum Constraints.DatumInline
        stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator racersVal
      <> Lookups.unspentOutputs
        ( Map.union (Map.singleton adminTxi adminTxo)
            (Map.singleton stateTxi stateTxo)
        )

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

-- | Given parameters attempts to get current onchain state/prices
queryRacersState
  :: Racers
       (RacersState /\ TransactionInput /\ TransactionOutputWithRefScript)
queryRacersState = do
  vhash <- validatorHash <$> mkRacersStateValidator
  rp <- asks _.params
  let
    stateAssetClass = (unwrap rp).stateToken
    scriptAddress = scriptHashAddress vhash Nothing
    stateVal = uncurry Value.singleton stateAssetClass one
  (stateTxi /\ stateTxo /\ rs) <- lift do
    scriptUtxos <- utxosAt scriptAddress
    stateTxi /\ stateTxo <-
      liftContractM "Could not find utxos with state token"
        $ find
            (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` stateVal)
        $ (Map.toUnfoldable scriptUtxos :: Array _)

    dat <-
      liftContractM "State UTxO does not contain datum or datum is not inline" $
        case (unwrap (unwrap stateTxo).output).datum of
          OutputDatum d -> Just d
          _ -> Nothing
    rs <- liftContractM "Could not deserialise into RacersState" $ fromData $
      unwrap
        dat
    pure (stateTxi /\ stateTxo /\ rs)
  pure $ rs /\ stateTxi /\ stateTxo

createRacersRefScriptOutput
  :: PlutusScript -> Racers TransactionInput
createRacersRefScriptOutput script = do
  stateValidatorHash <- validatorHash <$> mkRacersStateValidator

  let
    scriptRef :: ScriptRef
    scriptRef = PlutusScriptRef script

    constraints :: Constraints.TxConstraints Unit Unit
    constraints =
      Constraints.mustPayToScriptWithScriptRef stateValidatorHash unitDatum
        DatumWitness
        scriptRef
        (Value.lovelaceValueOf $ BigInt.fromInt 2_000_000)

    lookups :: Lookups.ScriptLookups PlutusData
    lookups = mempty

  lift do
    txHash <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txHash
    pure $ wrap
      { transactionId: txHash
      , index: zero
      }

queryRacersRefScriptOutput
  :: ScriptHash
  -> Racers (Maybe (TransactionInput /\ TransactionOutputWithRefScript))
queryRacersRefScriptOutput targetScriptHash = do
  stateValidator <- mkRacersStateValidator
  let stateAddress = scriptHashAddress (validatorHash stateValidator) Nothing
  utxos <- lift $ utxosAt stateAddress
  pure $ findMatchingScriptHash utxos
  where
  -- Check if the script hash in the transaction output matches the target script hash
  scriptHashMatches :: TransactionOutputWithRefScript -> Boolean
  scriptHashMatches txo =
    let
      output = unwrap (unwrap txo).output
      mRefScript = output.referenceScript
    in
      maybe false (_ == targetScriptHash) mRefScript

  -- Find the UTxO with a matching script hash, and return its index and value
  findMatchingScriptHash utxos = (\x -> x.index /\ x.value) <$> findWithIndex
    (const scriptHashMatches)
    utxos

mkRacersStateValidator :: Racers Validator
mkRacersStateValidator = do
  params <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope racersStateValidatorScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData params
  pure $ Validator $ appliedScript
