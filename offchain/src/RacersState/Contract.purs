module CardanoRacers.RacersState.Contract
  ( initRacersStateContract
  , modifyRacersStateContract
  , queryRacersState
  , createRacersRefScriptOutputs
  , queryRacersRefScriptOutput
  , mkRacersStateValidator
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.CurrencySymbol (toCardano) as Plutus
import Cardano.Plutus.Types.Validator (Validator(Validator))
import Cardano.Types
  ( Address
  , RedeemerDatum(RedeemerDatum)
  , TransactionHash
  , TransactionOutput
  , Value
  )
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Credential (Credential(ScriptHashCredential))
import Cardano.Types.OutputDatum (outputDatumDatum)
import Cardano.Types.PlutusScript (hash)
import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.RacersState.Types
  ( RacersState
  , RacersStateRedeemer(SetRacersState)
  )
import CardanoRacers.ScriptsFFI (racersStateValidatorScript)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (mkAddress)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (PlutusScript, ScriptHash)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( ScriptRef(NativeScriptRef, PlutusScriptRef)
  , TransactionInput
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
  vhash <- (hash <<< unwrap) <$> mkRacersStateValidator
  rp <- asks _.params
  (scriptHash :: ScriptHash) <- lift
    $ liftContractM "Could get ScriptHash from Plutus' CurrencySymbol"
    $ Plutus.toCardano
    $ fst (unwrap rp).stateToken

  let
    datum = toData ns

    (stateVal :: Value) = Value.singleton scriptHash
      (unwrap $ snd (unwrap rp).stateToken)
      BigNum.one

    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustPayToScript vhash datum
      Constraints.DatumInline
      stateVal

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.validator (unwrap racersVal) <> Lookups.unspentOutputs
      utxos

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

-- | Given RacersParams and a state attempts to consume current state UTxO
-- | and create a new UTxO with the new state.
-- | throws if admin token is not present in wallet balance or if state token is
-- | not already locked at script
modifyRacersStateContract
  :: (RacersState -> RacersState) -> Racers TransactionHash
modifyRacersStateContract modifyState = do
  racersVal <- mkRacersStateValidator
  rp <- asks _.params

  (scriptHash :: ScriptHash) <- lift
    $ liftContractM "Could get ScriptHash from Plutus' CurrencySymbol"
    $ Plutus.toCardano
    $ fst (unwrap rp).stateToken

  (oldState /\ stateTxi /\ stateTxo) <- queryRacersState

  let
    newState = modifyState oldState
    vhash = hash $ unwrap racersVal
    datum = toData $ newState

    (stateVal :: Value) = Value.singleton scriptHash
      (unwrap $ snd (unwrap rp).stateToken)
      BigNum.one

    red = RedeemerDatum
      $ toData
      $ SetRacersState newState

  (adminTxi /\ adminTxo) <- findAnyAuthUtxo >>=
    (lift <<< liftContractM "Could not find admin token in wallet")

  let
    constraints :: Constraints.TxConstraints
    constraints = Constraints.mustSpendPubKeyOutput adminTxi
      <> Constraints.mustSpendScriptOutput stateTxi red
      <> Constraints.mustPayToScript vhash datum Constraints.DatumInline
        stateVal

    lookups :: Lookups.ScriptLookups
    lookups = Lookups.validator (unwrap racersVal)
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
       (RacersState /\ TransactionInput /\ TransactionOutput)
queryRacersState = do
  vhash <- (hash <<< unwrap) <$> mkRacersStateValidator
  (scriptAddress :: Address) <- lift $ mkAddress
    (wrap $ ScriptHashCredential $ vhash)
    Nothing
  (rp :: RacersParams) <- asks _.params
  (scriptHash :: ScriptHash) <- lift
    $ liftContractM "Could get ScriptHash from Plutus' CurrencySymbol"
    $ Plutus.toCardano
    $ fst (unwrap rp).stateToken

  let
    (stateVal :: Value) = Value.singleton scriptHash
      (unwrap $ snd (unwrap rp).stateToken)
      BigNum.one
  (stateTxi /\ stateTxo /\ rs) <- lift do
    scriptUtxos <- utxosAt scriptAddress
    stateTxi /\ stateTxo <-
      liftContractM "Could not find utxos with state token"
        $ find
            (\(_ /\ txo) -> (unwrap txo).amount `geq` stateVal)
        $ (Map.toUnfoldable scriptUtxos :: Array _)

    dat <-
      liftContractM "State UTxO does not contain datum or datum is not inline" $
        (unwrap stateTxo).datum

    datum <-
      liftContractM "Could not get PlutusData from OutputDatum" $
        outputDatumDatum dat

    rs <- liftContractM "Could not deserialise into RacersState" $
      ( (fromData $ datum) :: Maybe RacersState
      )

    pure (stateTxi /\ stateTxo /\ rs)
  pure $ rs /\ stateTxi /\ stateTxo

createRacersRefScriptOutputs
  :: Array PlutusScript -> Racers TransactionInput
createRacersRefScriptOutputs scripts = do
  stateValidatorHash <- (hash <<< unwrap) <$> mkRacersStateValidator

  let
    constraints :: Constraints.TxConstraints
    constraints = foldMap
      ( \script ->
          Constraints.mustPayToScriptWithScriptRef stateValidatorHash unitDatum
            DatumWitness
            (PlutusScriptRef script)
            (Value.lovelaceValueOf $ BigNum.fromInt 2_000_000)
      )
      scripts

    lookups :: Lookups.ScriptLookups
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
  -> Racers (Maybe (TransactionInput /\ TransactionOutput))
queryRacersRefScriptOutput targetScriptHash = do
  stateValidator <- mkRacersStateValidator
  (stateAddress :: Address) <- lift $ mkAddress
    (wrap $ ScriptHashCredential $ hash $ unwrap stateValidator)
    Nothing
  utxos <- lift $ utxosAt stateAddress
  pure $ findMatchingScriptHash utxos
  where
  -- Check if the script hash in the transaction output matches the target script hash
  scriptHashMatches :: TransactionOutput -> Boolean
  scriptHashMatches txo =
    let
      mRefScript = (unwrap txo).scriptRef
    in
      maybe false
        ( \(rf :: ScriptRef) -> case rf of
            NativeScriptRef _ -> false
            PlutusScriptRef ps -> hash ps == targetScriptHash
        )
        mRefScript

  -- Find the UTxO with a matching script hash, and return its index and value
  findMatchingScriptHash utxos = (\x -> x.index /\ x.value) <$> findWithIndex
    (const scriptHashMatches)
    utxos

mkRacersStateValidator :: Racers Validator
mkRacersStateValidator = do
  params <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope racersStateValidatorScript
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData params
  pure $ Validator $ appliedScript
