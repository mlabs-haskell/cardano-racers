module CardanoRacers.Nitro.Contract
  ( adminMintsNitroContract
  , botMintsNitroContract
  , buyNitroContract
  , mintNitroConstraints
  , mintNitroAndPayToAddressConstraints
  , mintNitroAndPayToAddressContract
  , paysNitroConstraints
  , mkNitroPolicy
  ) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import CardanoRacers.Helpers (paysToAddrConstraint)
import CardanoRacers.Nitro.Types
  ( NitroPolicyRedeemer(BuyNitroToken, MintNitroToken)
  )
import CardanoRacers.RacersState.Contract
  ( queryRacersRefScriptOutput
  , queryRacersState
  )
import CardanoRacers.ScriptsFFI (nitroMintingPolicyScript)
import Common.ContractHelpers (findOwnAuthUtxo)
import Contract.Address (Address)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM, throwContractError)
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
import Data.Array (singleton) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt, toNumber) as BigInt
import Data.Int (ceil)
import Data.Map (singleton) as Map
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

mintNitroConstraints
  :: RacersParams
  -> BigInt
  -> Contract
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintNitroConstraints rp nitroAmount = do
  nitroPolicy <- mkNitroPolicy rp
  (authTxi /\ authTxo) <- liftedM "could not find admin or bot utxo in wallet" $
    findOwnAuthUtxo rp

  mNitroPolicyRef <- queryRacersRefScriptOutput rp
    (unwrap $ mintingPolicyHash nitroPolicy)

  let
    red = Redeemer $ toData $ MintNitroToken nitroAmount

    mintConstraints /\ mintLookups = case mNitroPolicyRef of
      Nothing ->
        Constraints.mustMintCurrencyWithRedeemer
          (mintingPolicyHash nitroPolicy)
          red
          (unwrap rp).nitroToken
          nitroAmount
          /\ Lookups.mintingPolicy nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyWithRedeemerUsingScriptRef
          (mintingPolicyHash nitroPolicy)
          red
          (unwrap rp).nitroToken
          nitroAmount
          (RefInput $ mkTxUnspentOut refTxi refTxo) /\ mempty

    constraints :: Constraints.TxConstraints Void Void
    constraints = mintConstraints <> Constraints.mustSpendPubKeyOutput authTxi

    lookups :: Lookups.ScriptLookups Void
    lookups = mintLookups
      <> Lookups.unspentOutputs (Map.singleton authTxi authTxo)

  pure (constraints /\ lookups)

paysNitroConstraints
  :: RacersParams
  -> Address
  -> BigInt
  -> Contract (Constraints.TxConstraints Void Void)
paysNitroConstraints rp targetAddress nitroAmount = do
  nitroSymbol <- liftedM "Could not get currency symbol"
    $ scriptCurrencySymbol
    <$> mkNitroPolicy rp

  pure $ paysToAddrConstraint targetAddress
    (Value.singleton nitroSymbol (unwrap rp).nitroToken nitroAmount)

mintNitroAndPayToAddressConstraints
  :: RacersParams
  -> BigInt
  -> Address
  -> Contract
       (Constraints.TxConstraints Void Void /\ Lookups.ScriptLookups Void)
mintNitroAndPayToAddressConstraints rp nitroAmount targetAddress = do
  (mintConstraints /\ mintLookups) <- mintNitroConstraints rp nitroAmount
  payConstraints <- paysNitroConstraints rp targetAddress nitroAmount
  pure $ (payConstraints <> mintConstraints) /\ mintLookups

mintNitroAndPayToAddressContract
  :: RacersParams -> BigInt -> Address -> Contract TransactionHash
mintNitroAndPayToAddressContract rp nitroAmount targetAddress = do
  (constraints /\ lookups) <- mintNitroAndPayToAddressConstraints rp nitroAmount
    targetAddress
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

-- | Given script parameters and an amount, attempts to mint nitro token.
-- | throws if admin token is not present
mintNitroContract
  :: RacersParams
  -> BigInt
  -> Contract TransactionHash
mintNitroContract rp nitroAmount = do
  (constraints /\ lookups) <- mintNitroConstraints rp nitroAmount

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

adminMintsNitroContract
  :: RacersParams -> BigInt -> Contract TransactionHash
adminMintsNitroContract nsp nitroAmount = mintNitroContract
  -- (_.adminToken <<< unwrap)
  nsp
  nitroAmount

botMintsNitroContract
  :: RacersParams -> BigInt -> Contract TransactionHash
botMintsNitroContract nsp nitroAmount = mintNitroContract
  -- (_.botToken <<< unwrap)
  nsp
  nitroAmount

-- |  Given RacersParams and an amount attempts to purchase NitroToken based
-- |  on current onchain nitro price
buyNitroContract :: RacersParams -> BigInt -> Contract TransactionHash
buyNitroContract rp nitroAmount = do
  nitroPolicy <- mkNitroPolicy rp
  let
    red = Redeemer $ toData $ BuyNitroToken nitroAmount
  ns /\ stateTxi /\ stateTxo <- queryRacersState rp

  mNitroPolicyRef <- queryRacersRefScriptOutput rp
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
          (unwrap rp).nitroToken
          nitroAmount
          /\ Lookups.mintingPolicy nitroPolicy
      Just (refTxi /\ refTxo) ->
        Constraints.mustMintCurrencyUsingScriptRef
          (mintingPolicyHash nitroPolicy)
          (unwrap rp).nitroToken
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

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure txId

mkNitroPolicy :: RacersParams -> Contract MintingPolicy
mkNitroPolicy rp = do
  v2script <- liftContractM "Could not decode applied script" do
    envelope <- decodeTextEnvelope nitroMintingPolicyScript
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData rp
  pure $ PlutusMintingPolicy $ appliedScript
