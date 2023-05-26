module CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mintNitroConstraints
  , mintNitroAndPayToAddressConstraints
  , mintNitroAndPayToAddressContract
  , paysNitroConstraints
  , mkNitroPolicy
  , burnNitroConstraints
  ) where

import Contract.Prelude

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
import Contract.Monad (liftContractM)
import Contract.PlutusData (Redeemer(Redeemer), toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts
  ( MintingPolicy(PlutusMintingPolicy)
  , applyArgs
  , mintingPolicyHash
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionHash
  , awaitTxConfirmed
  , mkTxUnspentOut
  , submitTxFromConstraints
  )
import Contract.TxConstraints (InputWithScriptRef(RefInput))
import Contract.TxConstraints as Constraints
import Contract.Value (lovelaceValueOf, singleton) as Value
import Contract.Value (scriptCurrencySymbol)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)
import Racers (Racers)

mintNitroConstraints
  :: BigInt
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintNitroConstraints nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  (authTxi /\ authTxo) <-
    findAnyAuthUtxo >>=
      (lift <<< liftContractM "could not find admin or bot utxo in wallet")
  mNitroPolicyRef <- queryRacersRefScriptOutput
    (unwrap $ mintingPolicyHash nitroPolicy)

  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroAmount
          /\ Lookups.mintingPolicy nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroAmount
          (RefInput $ mkTxUnspentOut refTxi refTxo) /\ mempty

    constraints :: Constraints.TxConstraints Void Void
    constraints = mintConstraints <> Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = mintLookups
      <> Lookups.unspentOutputs (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

burnNitroConstraints
  :: BigInt
  -> Racers (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
burnNitroConstraints nitroAmount = do
  nitroPolicy <- mkNitroPolicy
  mNitroPolicyRef <- queryRacersRefScriptOutput
    (unwrap $ mintingPolicyHash nitroPolicy)

  let
    red = Redeemer $ toData $ BurnNitroToken

    nitroToMint = negate nitroAmount

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroToMint
          /\ Lookups.mintingPolicy nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroToMint
          (RefInput $ mkTxUnspentOut refTxi refTxo) /\ mempty
  pure (mintConstraints /\ mintLookups)

paysNitroConstraints
  :: Address
  -> BigInt
  -> Racers (Constraints.TxConstraints Void Void)
paysNitroConstraints targetAddress nitroAmount = do
  nitroSymbol <-
    (scriptCurrencySymbol <$> mkNitroPolicy) >>=
      (lift <<< liftContractM "Could not get currency symbol")
  pure $ paysToAddrConstraint targetAddress
    (Value.singleton nitroSymbol nitroToken nitroAmount)

mintNitroAndPayToAddressConstraints
  :: BigInt
  -> Address
  -> Racers
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintNitroAndPayToAddressConstraints nitroAmount targetAddress = do
  (mintConstraints /\ mintLookups) <- mintNitroConstraints nitroAmount
  payConstraints <- paysNitroConstraints targetAddress nitroAmount
  pure $ (payConstraints <> mintConstraints) /\ mintLookups

mintNitroAndPayToAddressContract
  :: BigInt -> Address -> Racers TransactionHash
mintNitroAndPayToAddressContract nitroAmount targetAddress = do
  (constraints /\ lookups) <- mintNitroAndPayToAddressConstraints nitroAmount
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
  (constraints /\ lookups) <- mintNitroConstraints nitroAmount
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
  let
    red = Redeemer $ toData $ BuyNitroToken nitroAmount
  ns /\ stateTxi /\ stateTxo <- queryRacersState

  mNitroPolicyRef <- queryRacersRefScriptOutput
    (unwrap $ mintingPolicyHash nitroPolicy)

  let
    totalAmount = (unwrap ns).nitroPrice * nitroAmount
    treasuryAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.75
    operatingAmt = BigInt.fromInt <<< ceil $ BigInt.toNumber totalAmount * 0.25
    treasuryVal = Value.lovelaceValueOf treasuryAmt
    operatingVal = Value.lovelaceValueOf operatingAmt

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroAmount
          /\ Lookups.mintingPolicy nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (mintingPolicyHash nitroPolicy)
          red
          nitroToken
          nitroAmount
          (RefInput $ mkTxUnspentOut refTxi refTxo) /\ mempty

    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustReferenceOutput stateTxi
        <> paysToAddrConstraint (unwrap ns).treasuryAddress treasuryVal
        <> paysToAddrConstraint (unwrap ns).operatingAddress operatingVal
        <> mintConstraints

    lookups :: Lookups.ScriptLookups Void
    lookups = mintLookups
      <> Lookups.unspentOutputs (Map.singleton stateTxi stateTxo)

  lift do
    txId <- submitTxFromConstraints lookups constraints
    awaitTxConfirmed txId
    pure txId

mkNitroPolicy :: Racers MintingPolicy
mkNitroPolicy = do
  rp <- asks _.params
  v2script <- lift $ liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroMintingPolicyScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData rp
  pure $ PlutusMintingPolicy $ appliedScript
