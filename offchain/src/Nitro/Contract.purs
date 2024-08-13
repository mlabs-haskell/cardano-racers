module CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mintNitroConstraints
  , mintNitroContract
  , mintNitroAndPayToAddressConstraints
  , mintNitroAndPayToAddressContract
  , paysNitroConstraints
  , mkNitroPolicy
  , burnNitroConstraints
  ) where

import Contract.Prelude

import Cardano.FromData (fromData)
import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.Address as Address
import Cardano.ToData (toData)
import Cardano.Types (TransactionOutput)
import Cardano.Types.BigInt (fromString) as CTBigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int as Int
import Cardano.Types.PlutusScript (hash)
import CardanoRacers.Common.Types (nitroToken)
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(BuyNitroToken, BurnNitroToken, MintNitroToken)
  )
import CardanoRacers.RacersState.Contract
  ( queryRacersRefScriptOutput
  , queryRacersState
  )
import CardanoRacers.ScriptsFFI (nitroMintingPolicyScript)
import Common.ContractHelpers (findAnyAuthUtxo)
import Contract.Address (Address)
import Contract.Monad (liftContractM, liftedM)
import Contract.PlutusData (RedeemerDatum(RedeemerDatum))
import Contract.ScriptLookups (ScriptLookups, plutusMintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf, singleton) as Value
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromString, toNumber, toString) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import JS.BigInt as JSBigInt
import Partial.Unsafe (unsafePartial)
import Racers (Racers, withContract)

mintNitroConstraints
  :: (TransactionInput /\ TransactionOutput)
  -> BigInt
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
mintNitroConstraints (authTxi /\ authTxo) nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  nitroSHash <- lift $ liftContractM "Could not get script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)
  mNitroPolicyRef <- queryRacersRefScriptOutput nitroSHash

  nitroAmountBG <- lift
    $ liftContractM "Could not convert nitroAmount to BigNum"
    $ CTBigInt.fromString
    $ BigInt.toString nitroAmount

  nitroAmountI <- lift $ liftContractM "Could not convert nitroAmount to Int"
    $ Int.fromString
    $ BigInt.toString nitroAmount

  let
    red = RedeemerDatum $ toData $ MintNitroToken nitroAmountBG

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          nitroSHash
          red
          (unwrap nitroToken)
          nitroAmountI
          /\ nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          nitroSHash
          red
          (unwrap nitroToken)
          nitroAmountI
          (RefInput $ wrap { input: refTxi, output: refTxo }) /\ mempty

    constraints :: Constraints.TxConstraints
    constraints = mintConstraints <> Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups
    lookups = mintLookups
      <> Lookups.unspentOutputs (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

burnNitroConstraints
  :: BigInt
  -> Racers (Constraints.TxConstraints /\ Lookups.ScriptLookups)
burnNitroConstraints nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  nitroSHash <- lift $ liftContractM "Could not get script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)
  mNitroPolicyRef <- queryRacersRefScriptOutput nitroSHash

  let
    red = RedeemerDatum $ toData $ BurnNitroToken

    nitroToMint = negate nitroAmount

  nitroToMintI <- lift $ liftContractM "Could not convert nitroToMint to BigInt"
    $ Int.fromString
    $ BigInt.toString nitroToMint

  let
    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          nitroSHash
          red
          (unwrap nitroToken)
          nitroToMintI
          /\ nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          nitroSHash
          red
          (unwrap nitroToken)
          nitroToMintI
          (RefInput $ wrap { input: refTxi, output: refTxo }) /\ mempty
  pure (mintConstraints /\ mintLookups)

paysNitroConstraints
  :: Address
  -> BigInt
  -> Racers (Constraints.TxConstraints)
paysNitroConstraints targetAddress nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  nitroSymbol <- lift $ liftContractM "Could not get script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)
  let
    addr = unsafePartial $ fromJust $
      Address.fromCardano
        targetAddress
  nitroAmountBG <- lift
    $ liftContractM "Could not convert nitroAmount to BigNum"
    $ BigNum.fromString
    $ BigInt.toString nitroAmount
  pure $ paysToAddrConstraint addr
    (Value.singleton nitroSymbol (unwrap nitroToken) nitroAmountBG)

mintNitroAndPayToAddressConstraints
  :: (TransactionInput /\ TransactionOutput)
  -> BigInt
  -> Address
  -> Racers
       (Constraints.TxConstraints /\ Lookups.ScriptLookups)
mintNitroAndPayToAddressConstraints
  (authTxi /\ authTxo)
  nitroAmount
  targetAddress = do
  (mintConstraints /\ mintLookups) <- mintNitroConstraints (authTxi /\ authTxo)
    nitroAmount
  payConstraints <- paysNitroConstraints targetAddress nitroAmount
  pure $ (payConstraints <> mintConstraints) /\ mintLookups

mintNitroAndPayToAddressContract
  :: BigInt -> Address -> Racers TransactionHash
mintNitroAndPayToAddressContract nitroAmount targetAddress = do
  authInput <- withContract (liftedM "Could not find auth tokens in wallet")
    findAnyAuthUtxo
  (constraints /\ lookups) <- mintNitroAndPayToAddressConstraints authInput
    nitroAmount
    targetAddress
  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

-- | Given script parameters and an amount, attempts to mint nitro token.
-- | throws if admin token is not present
mintNitroContract
  :: BigInt
  -> Racers TransactionHash
mintNitroContract nitroAmount = do
  authInput <- withContract (liftedM "Could not find auth tokens in wallet")
    findAnyAuthUtxo
  (constraints /\ lookups) <- mintNitroConstraints authInput nitroAmount
  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

adminMintsNitroContract
  :: BigInt -> Racers TransactionHash
adminMintsNitroContract nitroAmount = mintNitroContract nitroAmount

botMintsNitroContract
  :: BigInt -> Racers TransactionHash
botMintsNitroContract nitroAmount = mintNitroContract nitroAmount

-- |  Given RacersParams and an amount attempts to purchase NitroToken based
-- |  on current onchain nitro price
buyNitroContract :: BigInt -> Racers TransactionHash
buyNitroContract nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  nitroSHash <- lift $ liftContractM "Could not get script hash"
    $ hash
    <$> (head (unwrap nitroPolicy).plutusMintingPolicies)

  nitroAmountBG <- lift
    $ liftContractM "Could not convert nitroAmount to BigNum"
    $ CTBigInt.fromString
    $ BigInt.toString nitroAmount

  nitroAmountI <- lift $ liftContractM "Could not convert nitroAmount to Int"
    $ Int.fromString
    $ BigInt.toString nitroAmount

  red <- lift
    $ liftContractM "Could not get Redeemer data"
    $ fromData
    $ toData
    $ BuyNitroToken nitroAmountBG

  ns /\ stateTxi /\ stateTxo <- queryRacersState

  nitroPriceI <- lift $ liftContractM "Could not convert nitroPrice to BigInt"
    $ BigInt.fromString
    $ JSBigInt.toString (unwrap ns).nitroPrice

  mNitroPolicyRef <- queryRacersRefScriptOutput nitroSHash

  let
    (totalAmount :: BigInt) = nitroPriceI * nitroAmount

    (treasuryAmt :: BigNum.BigNum) = BigNum.fromInt <<< ceil
      $ BigInt.toNumber totalAmount
      * 0.75

    (operatingAmt :: BigNum.BigNum) = BigNum.fromInt <<< ceil
      $ BigInt.toNumber totalAmount
      * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          nitroSHash
          red
          (unwrap nitroToken)
          nitroAmountI
          /\ nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          nitroSHash
          red
          (unwrap nitroToken)
          nitroAmountI
          (RefInput $ wrap { input: refTxi, output: refTxo }) /\ mempty

    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap ns).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap ns).operatingAddress operatingVal
        <> mintConstraints

    lookups :: Lookups.ScriptLookups
    lookups = mintLookups
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkNitroPolicy :: Racers ScriptLookups
mkNitroPolicy = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroMintingPolicyScript
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData rp
  pure $ plutusMintingPolicy $ appliedScript
