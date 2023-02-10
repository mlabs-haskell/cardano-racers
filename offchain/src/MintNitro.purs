module NitroMint
  ( NitroScriptParams(NitroScriptParams)
  , NitroState(NitroState)
  , mintNitroContract
  , buyNitroContract
  , initNitroStateContract
  , modifyNitroStateContract
  , mkNitroValidator
  , mkNitroPolicy
  ) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (rawNitroMintingPolicy)
import Contract.Address (Address, scriptHashAddress)
import Contract.Credential (Credential(..))
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.Numeric.Rational (reduce, (%))
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , Datum(..)
  , I
  , OutputDatum(..)
  , PNil
  , Redeemer(Redeemer)
  , S
  , Z
  , fromData
  , genericFromData
  , genericToData
  , toData
  , unitDatum
  )
import Contract.ScriptLookups (mkUnbalancedTx)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , PlutusScript
  , Validator(Validator)
  , ValidatorHash(..)
  , applyArgs
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionInput(..)
  , TransactionOutputWithRefScript(..)
  , awaitTxConfirmed
  , balanceTx
  , submitTxFromConstraints
  )
import Contract.TxConstraints (DatumPresence(..))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , geq
  , scriptCurrencySymbol
  )
import Contract.Value (lovelaceValueOf, singleton) as Value
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, fromNumber, toNumber) as BigInt
import Data.Map (singleton, toUnfoldable, union) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

newtype NitroScriptParams = NitroScriptParams
  { adminToken :: (CurrencySymbol /\ TokenName)
  , stateToken :: (CurrencySymbol /\ TokenName)
  , nitroToken :: TokenName
  }

derive instance Generic NitroScriptParams _
derive instance Newtype NitroScriptParams _

instance
  HasPlutusSchema NitroScriptParams
    ( "NitroScriptParams"
        :=
          ( "adminToken" := I (CurrencySymbol /\ TokenName)
              :+ "stateToken"
              := I (CurrencySymbol /\ TokenName)
              :+ "nitroToken"
              := I TokenName
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData NitroScriptParams where
  toData = genericToData

instance FromData NitroScriptParams where
  fromData = genericFromData

newtype NitroState = NitroState
  { nitroPrice :: BigInt -- Nitro price in Lovelace
  , treasuryAddress :: Address
  , operatingAddress :: Address
  }

derive instance Generic NitroState _
derive instance Newtype NitroState _

instance
  HasPlutusSchema NitroState
    ( "NitroState"
        :=
          ( "nitroPrice" := I BigInt
              :+ "treasuryAddress"
              := I Address
              :+ "operatingAddress"
              := I Address
              :+ PNil
          )
        @@ Z
        :+ PNil
    )

instance ToData NitroState where
  toData = genericToData

instance FromData NitroState where
  fromData = genericFromData

data NitroScriptRedeemer
  = SetNitroState NitroState -- Requires AdminToken
  | MintNitroToken BigInt
  | BuyNitroToken BigInt

derive instance Generic NitroScriptRedeemer _
instance
  HasPlutusSchema NitroScriptRedeemer
    ( "SetNitroState" := PNil @@ Z
        :+ "MintNitroToken"
        := PNil
        @@ (S Z)
        :+ "BuyNitroToken"
        := PNil
        @@ (S (S Z))
        :+ PNil
    )

instance ToData NitroScriptRedeemer where
  toData = genericToData

instance FromData NitroScriptRedeemer where
  fromData = genericFromData

initNitroStateContract :: NitroScriptParams -> NitroState -> Contract () Unit
initNitroStateContract np ns = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroVal <- mkNitroValidator np
  let
    datum = Datum $ toData ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one

  let
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
  pure unit

modifyNitroStateContract :: NitroScriptParams -> NitroState -> Contract () Unit
modifyNitroStateContract np ns = do
  ownUtxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroVal <- mkNitroValidator np
  let
    vhash = validatorHash nitroVal
    datum = Datum $ toData ns
    red = Redeemer $ toData $ SetNitroState ns -- $ wrap $ (unwrap ns) { nitroPrice= BigInt.fromInt 1000000}
    stateVal = uncurry Value.singleton (unwrap np).stateToken one
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
    scriptAddr = scriptHashAddress vhash Nothing
  (adminTxi /\ _) <- liftContractM "admin token not in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable ownUtxos :: Array _)
  (stateTxi /\ stateTxo) <- liftedM "Couldn't find state token at script"
    $ utxosAt scriptAddr
    <#> (Map.toUnfoldable :: _ -> Array _)
    <#> find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` stateVal)
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
  pure $ unit

mintNitroContract :: BigInt -> NitroScriptParams -> Contract () Unit
mintNitroContract nitroAmount np = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroMp <- mkNitroPolicy np
  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
  (adminTxi /\ _) <- liftContractM "admin token not in wallet"
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
  pure $ unit

buyNitroContract :: BigInt -> NitroScriptParams -> Contract () Unit
buyNitroContract nitroAmount np = do
  nitroVal <- mkNitroValidator np
  nitroMp <- mkNitroPolicy np
  let
    vhash = validatorHash nitroVal
    red = Redeemer $ toData $ BuyNitroToken nitroAmount
  (ns /\ stateTxi /\ stateTxo) <- getCurrentNitroState (unwrap np).stateToken
    vhash
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ nitroMp

  let
    totalAmount = (unwrap ns).nitroPrice * nitroAmount
  treasuryAmt <- liftContractM "Couldn't convert to BigInt"
    $ BigInt.fromNumber
    $ BigInt.toNumber totalAmount
    * 0.75
  operatingAmt <- liftContractM "Couldn't convert to BigInt"
    $ BigInt.fromNumber
    $ BigInt.toNumber totalAmount
    * 0.25
  let
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
  pure $ unit

getCurrentNitroState
  :: (CurrencySymbol /\ TokenName)
  -> ValidatorHash
  -> Contract ()
       (NitroState /\ TransactionInput /\ TransactionOutputWithRefScript)
getCurrentNitroState stateAssetClass vhash = do
  let
    scriptAddress = scriptHashAddress vhash Nothing
    stateVal = uncurry Value.singleton stateAssetClass one
  scriptUtxos <- utxosAt scriptAddress
  (stateTxi /\ stateTxo) <- liftContractM "Couldn't find utxos with state token"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` stateVal)
    $ (Map.toUnfoldable scriptUtxos :: Array _)
  logInfo' $ show stateTxo
  dat <- liftContractM "OutputDatum is not inline" $
    case (unwrap (unwrap stateTxo).output).datum of
      OutputDatum d -> Just d
      _ -> Nothing
  ns <- liftContractM "Couldn't deserialise into NitroState" $ fromData $ unwrap
    dat
  pure $ ns /\ stateTxi /\ stateTxo

mkNitroValidator :: NitroScriptParams -> Contract () Validator
mkNitroValidator np = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope rawNitroMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ Validator $ appliedScript

mkNitroPolicy :: NitroScriptParams -> Contract () MintingPolicy
mkNitroPolicy np = do
  valScript <- mkNitroValidator np
  appliedScript <- liftEither $ left (error <<< show)
    $ applyArgs (unwrap valScript)
    $ Array.singleton
    $ toData unitDatum
  pure $ PlutusMintingPolicy appliedScript
