module CardanoRacers.Nitro.Contract
  ( mintNitroContract
  , buyNitroContract
  , initNitroStateContract
  , modifyNitroStateContract
  , queryNitroPolicyState
  , mkNitroValidator
  , mkNitroPolicy
  ) where

import Contract.Prelude

import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(BuyNitroToken, MintNitroToken)
  , NitroScriptParams
  , NitroState
  , NitroStateRedeemer(SetNitroState)
  )
import CardanoRacers.ScriptsFFI
  ( nitroMintingPolicyScript
  , nitroStateValidatorScript
  )
import Contract.Address (Address, scriptHashAddress)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData
  ( Datum(Datum)
  , OutputDatum(OutputDatum)
  , Redeemer(Redeemer)
  , fromData
  , toData
  , unitDatum
  )
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , Validator(Validator)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , TransactionOutputWithRefScript
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (Value, geq, scriptCurrencySymbol)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable, union) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

-- | Given NitroScriptParams attempts to lock the StateToken with an inline
-- | NitroState datum at validator script
-- | throws InsufficientTxInputs if state token is not in current wallets
-- | balance
initNitroStateContract
  :: NitroScriptParams -> NitroState -> Contract () TransactionHash
initNitroStateContract np ns = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroVal <- mkNitroValidator np
  let
    datum = Datum $ toData ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one

    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript
      (validatorHash nitroVal)
      datum
      Constraints.DatumInline
      stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator nitroVal
      <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given NitroScriptParams and a state attempts to consume current state UTxO
-- | and create a new UTxO with the new state.
-- | throws if admin token is not present in wallet balance or if state token is
-- | not already locked at script
modifyNitroStateContract
  :: NitroScriptParams -> NitroState -> Contract () TransactionHash
modifyNitroStateContract np ns = do
  ownUtxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroVal <- mkNitroValidator np
  let
    vhash = validatorHash nitroVal
    datum = Datum $ toData ns
    red = Redeemer $ toData $ SetNitroState ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
  (adminTxi /\ _) <- liftContractM "Could not find admin token in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable ownUtxos :: Array _)
  (_ /\ stateTxi /\ stateTxo) <- queryNitroPolicyState np
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput adminTxi
      <> Constraints.mustSpendScriptOutput stateTxi red
      <> Constraints.mustPayToScript vhash datum Constraints.DatumInline
        stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator nitroVal
      <> Lookups.unspentOutputs
        (Map.union ownUtxos (Map.singleton stateTxi stateTxo))

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given script parameters and an amount, attempts to mint nitro token.
-- | throws if admin token is not present
mintNitroContract :: NitroScriptParams -> BigInt -> Contract () TransactionHash
mintNitroContract np nitroAmount = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroMp <- mkNitroPolicy np
  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
  adminTxi /\ _ <- liftContractM "admin token not in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable utxos :: Array _)
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ nitroMp
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer red
        (Value.singleton cs (unwrap np).nitroToken nitroAmount)
        <> Constraints.mustSpendPubKeyOutput adminTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy nitroMp
      <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- |  Given NitroScriptParams and an amount attempts to purchase NitroToken based
-- |  on current onchain nitro price
buyNitroContract :: NitroScriptParams -> BigInt -> Contract () TransactionHash
buyNitroContract np nitroAmount = do
  nitroMp <- mkNitroPolicy np
  let
    red = Redeemer $ toData $ BuyNitroToken nitroAmount
  ns /\ stateTxi /\ stateTxo <- queryNitroPolicyState np
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ nitroMp

  let
    totalAmount = (unwrap ns).nitroPrice * nitroAmount
    treasuryAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.75
    operatingAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt

    paysToAddrConstraint
      :: Address -> Value -> Constraints.TxConstraints Void Void
    paysToAddrConstraint a v = case (unwrap a).addressCredential of
      PubKeyCredential pkh ->
        Constraints.mustPayToPubKey (wrap pkh) v
      ScriptCredential vh ->
        Constraints.mustPayToScript vh unitDatum DatumWitness v

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap ns).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap ns).operatingAddress operatingVal
        <> Constraints.mustMintValueWithRedeemer red
          (Value.singleton cs (unwrap np).nitroToken nitroAmount)

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy nitroMp
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given nitro parameters attempts to get current onchain nitro state/price
queryNitroPolicyState
  :: NitroScriptParams
  -> Contract ()
       (NitroState /\ TransactionInput /\ TransactionOutputWithRefScript)
queryNitroPolicyState nsp = do
  vhash <- validatorHash <$> mkNitroValidator nsp
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
  ns <- liftContractM "Could not deserialise into NitroState" $ fromData $
    unwrap
      dat
  pure $ ns /\ stateTxi /\ stateTxo

mkNitroValidator :: NitroScriptParams -> Contract () Validator
mkNitroValidator np = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroStateValidatorScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ Validator $ appliedScript

mkNitroPolicy :: NitroScriptParams -> Contract () MintingPolicy
mkNitroPolicy np = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroMintingPolicyScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ PlutusMintingPolicy $ appliedScript
