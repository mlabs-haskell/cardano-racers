module NitroMint
  ( NitroScriptParams(NitroScriptParams)
  , NitroState(NitroState)
  , mintNitroContract
  , buyNitroContract
  , initNitroStateContract
  , modifyNitroStateContract
  ) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (rawNitroMintingPolicy)
import Contract.Address
  ( Address
  , getNetworkId
  , getWalletAddresses
  , scriptHashAddress
  , validatorHashEnterpriseAddress
  )
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
import Contract.PlutusData
  ( class FromData
  , class HasPlutusSchema
  , class ToData
  , type (:+)
  , type (:=)
  , type (@@)
  , Datum(..)
  , I
  , PNil
  , Redeemer(..)
  , S
  , Z
  , fromData
  , genericFromData
  , genericToData
  , toData
  , unitDatum
  )
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(..)
  , PlutusScript(..)
  , Validator(..)
  , applyArgs
  , mintingPolicyHash
  , validatorHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( Redeemer
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constrains
import Contract.TxConstraints as Constraints
import Contract.TxConstraints as TxConstraints
import Contract.Utxos (getWalletUtxos, utxosAt)
import Contract.Value (CurrencySymbol, TokenName, geq, scriptCurrencySymbol)
import Contract.Value (mkTokenName, scriptCurrencySymbol, singleton, valueOf) as Value
import Data.Array (head, singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
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

  nitroScript <- mkNitroPolicy np

  let
    datum = Datum $ toData ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustPayToScript
      (validatorHash $ Validator nitroScript)
      datum
      TxConstraints.DatumInline
      stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator (Validator nitroScript)
      <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  -- balAtVal <- utxosAt (scriptHashAddress (wrap $ unwrap $ mintingPolicyHash mp) Nothing)
  -- logInfo' $ show balAtVal
  pure unit

modifyNitroStateContract :: NitroScriptParams -> NitroState -> Contract () Unit
modifyNitroStateContract np ns = do
  ownUtxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  netId <- getNetworkId
  nitroScript <- mkNitroPolicy np
  let
    nitroValidator = Validator nitroScript
    valHash = validatorHash nitroValidator
    datum = Datum $ toData ns
    red = Redeemer $ toData $ SetNitroState ns
    stateVal = uncurry Value.singleton (unwrap np).stateToken one
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
  valAddr <- liftContractM "Couldn't get validator address" $
    validatorHashEnterpriseAddress netId valHash
  (adminTxi /\ _) <- liftContractM "admin token not in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable ownUtxos :: Array _)
  (stateTxi /\ stateTxo) <- liftedM "Couldn't find state token at script"
    $ utxosAt valAddr
    <#> (Map.toUnfoldable :: _ -> Array _)
    <#> find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` stateVal)
  logInfo' $ show stateTxo
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints = Constraints.mustSpendPubKeyOutput adminTxi
      <> Constraints.mustSpendScriptOutput stateTxi red
      <> Constraints.mustPayToScript valHash datum TxConstraints.DatumInline
        stateVal

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.validator nitroValidator
      <> Lookups.unspentOutputs
        (Map.union ownUtxos (Map.singleton stateTxi stateTxo))

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ unit

mintNitroContract :: BigInt -> NitroScriptParams -> Contract () Unit
mintNitroContract nitroAmount np = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  mp <- PlutusMintingPolicy <$> mkNitroPolicy np
  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount
    adminVal = uncurry Value.singleton (unwrap np).adminToken one
  (adminTxi /\ txo) <- liftContractM "admin token not in wallet"
    $ find (\(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq` adminVal)
    $ (Map.toUnfoldable utxos :: Array _)
  logInfo' $ show $ adminTxi /\ txo
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ mp
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer red
        (Value.singleton cs (unwrap np).nitroToken nitroAmount)
        <> Constraints.mustSpendPubKeyOutput adminTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy mp
      <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ unit

buyNitroContract :: NitroScriptParams -> Contract () Unit
buyNitroContract np = pure unit

mkNitroPolicy :: NitroScriptParams -> Contract () PlutusScript
mkNitroPolicy np = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope rawNitroMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ appliedScript

-- mkNitroMintingPolicy :: NitroScriptParams -> Contract () MintingPolicy
-- mkNitroMintingPolicy np = do
--   v2script <- liftContractM "Error decoding alwaysSucceeds" do
--     envelope <- decodeTextEnvelope rawNitroMintingPolicy
--     plutusScriptV2FromEnvelope envelope
--   appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
--     $ [toData np, toData unitDatum]
--   pure $ PlutusMintingPolicy appliedScript
