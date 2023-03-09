module CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mkNitroPolicy
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Nitro.Types (NitroPolicyRedeemer(BuyNitroToken, MintNitroToken))
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.RacersState.Contract (queryRacersState)
import CardanoRacers.ScriptsFFI (nitroMintingPolicyScript)
import Contract.Address (Address)
import Contract.Credential (Credential(PubKeyCredential, ScriptCredential))
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (Redeemer(Redeemer), toData, unitDatum)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionHash, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints (DatumPresence(DatumWitness))
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, Value, geq, scriptCurrencySymbol)
import Contract.Value (lovelaceValueOf, singleton) as Value
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton, toUnfoldable) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

-- | Given script parameters and an amount, attempts to mint nitro token.
-- | throws if admin token is not present
mintNitroContract
  :: (RacersParams -> (CurrencySymbol /\ TokenName))
  -> RacersParams
  -> BigInt
  -> Contract TransactionHash
mintNitroContract authTokenGetter np nitroAmount = do
  utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
  nitroMp <- mkNitroPolicy np
  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount
    authTokenVal = uncurry Value.singleton (authTokenGetter np) one
  authTxi /\ _ <-
    liftContractM "Could not find appropriate auth token in wallet"
      $ find
          ( \(_ /\ txo) -> (unwrap (unwrap txo).output).amount `geq`
              authTokenVal
          )
      $ (Map.toUnfoldable utxos :: Array _)
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ nitroMp
  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValueWithRedeemer red
        (Value.singleton cs (unwrap np).nitroToken nitroAmount)
        <> Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = Lookups.mintingPolicy nitroMp
      <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

adminMintsNitroContract
  :: RacersParams -> BigInt -> Contract TransactionHash
adminMintsNitroContract nsp nitroAmount = mintNitroContract
  (_.adminToken <<< unwrap)
  nsp
  nitroAmount

botMintsNitroContract
  :: RacersParams -> BigInt -> Contract TransactionHash
botMintsNitroContract nsp nitroAmount = mintNitroContract
  (_.botToken <<< unwrap)
  nsp
  nitroAmount

-- |  Given RacersParams and an amount attempts to purchase NitroToken based
-- |  on current onchain nitro price
buyNitroContract :: RacersParams -> BigInt -> Contract TransactionHash
buyNitroContract np nitroAmount = do
  nitroMp <- mkNitroPolicy np
  let
    red = Redeemer $ toData $ BuyNitroToken nitroAmount
  ns /\ stateTxi /\ stateTxo <- queryRacersState np
  cs <- liftContractM "Could not get currency symbol"
    $ scriptCurrencySymbol
    $ nitroMp

  let
    totalAmount = (unwrap ns).nitroPrice * nitroAmount
    treasuryAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.75
    operatingAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt

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

mkNitroPolicy :: RacersParams -> Contract MintingPolicy
mkNitroPolicy np = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroMintingPolicyScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData np
  pure $ PlutusMintingPolicy $ appliedScript
