-- | This module contains a contract that mints an nft using a utxo from user
-- | wallet and CardanoRacersAdminNFT as the token name
module CardanoRacers.Nft
  ( mkNftMintingPolicy
  , mintManyNfts
  , mintNft
  ) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM, wrapContract)
import Contract.PlutusData (toData)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value (CurrencySymbol, TokenName, scriptCurrencySymbol)
import Contract.Value (singleton) as Value
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.QueryM.Kupo (getUtxoByOref)
import Data.Array (zip) as Array
import Data.Map (singleton)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

mintNftConstraints
  :: TransactionInput
  -> TokenName
  -> Contract ()
       ( CurrencySymbol /\ Constraints.TxConstraints Void Void /\
           Lookups.ScriptLookups Void
       )
mintNftConstraints txi tkname = do
  txo <- liftedM "Could not get utxos" $ liftedE $ wrapContract $ getUtxoByOref
    txi

  -- todo: find workaround to avoid using internal functions
  ptxo <- liftContractM "Could not convert to plutus txo" $
    toPlutusTxOutputWithRefScript txo

  mp <- mkNftMintingPolicy txi tkname
  cs <- liftContractM "couldn't get currency symbol" $ scriptCurrencySymbol mp

  let
    constraints :: Constraints.TxConstraints Void Void
    constraints =
      Constraints.mustMintValue (Value.singleton cs tkname one)
        <> Constraints.mustSpendPubKeyOutput txi

    lookups :: Lookups.ScriptLookups Void
    lookups =
      Lookups.mintingPolicy mp
        <> Lookups.unspentOutputs (singleton txi ptxo)

  pure $ cs /\ constraints /\ lookups

mintManyNfts
  :: TransactionInput
  -> Array TokenName
  -> Contract () (Array (CurrencySymbol /\ TokenName))
mintManyNfts txi tks = do
  nftConstraints <- traverse (mintNftConstraints txi) tks
  let
    constraints = foldMap (\(_ /\ c /\ _) -> c) nftConstraints
    lookups = foldMap (\(_ /\ _ /\ l) -> l) nftConstraints
    symbols = map (\(cs /\ _ /\ _) -> cs) nftConstraints
    assets = Array.zip symbols tks
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ assets

mintNft
  :: TransactionInput -> TokenName -> Contract () (CurrencySymbol /\ TokenName)
mintNft txi tk = do
  cs /\ constraints /\ lookups <- mintNftConstraints txi tk
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ cs /\ tk

mkNftMintingPolicy :: TransactionInput -> TokenName -> Contract () MintingPolicy
mkNftMintingPolicy txin tk = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope adminNftMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData txin, toData tk ]
  pure $ PlutusMintingPolicy appliedScript

