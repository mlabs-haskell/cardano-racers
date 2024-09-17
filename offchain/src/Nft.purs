-- | This module contains a contract that mints an nft using a utxo from user
-- | wallet and CardanoRacersAdminNFT as the token name
module CardanoRacers.Nft
  ( mkNftMintingPolicy
  , mintManyNfts
  , mintNftConstraints
  , mintNft
  ) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Types.Int as Int
import Cardano.Types.Mint (Mint, singleton) as Mint
import Cardano.Types.PlutusScript (hash) as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (toData)
import Contract.ScriptLookups (ScriptLookups, plutusMintingPolicy)
import Contract.ScriptLookups as Lookups
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Utxos (getUtxo)
import Contract.Value (CurrencySymbol, TokenName)
import Data.Array (head)
import Data.Array (zip) as Array
import Data.Map (singleton)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

mintNftConstraints
  :: TransactionInput
  -> TokenName
  -> Contract
       ( ScriptHash
           /\ Constraints.TxConstraints
           /\
             Lookups.ScriptLookups
       )
mintNftConstraints txi tkname = do
  txo <- liftedM "Could not get utxos" $ getUtxo txi

  mp <- mkNftMintingPolicy txi tkname
  mpHash <-
    liftContractM "Could not get race slot token script hash"
      $ head
      $ map PlutusScript.hash
      $ (unwrap mp).plutusMintingPolicies

  let
    amountToMint :: Mint.Mint
    amountToMint = Mint.singleton mpHash tkname Int.one

    constraints :: Constraints.TxConstraints
    constraints =
      Constraints.mustMintValue amountToMint
        <> Constraints.mustSpendPubKeyOutput txi

    lookups :: Lookups.ScriptLookups
    lookups = mp
      <> Lookups.unspentOutputs (singleton txi txo)

  pure $ mpHash /\ constraints /\ lookups

mintManyNfts
  :: TransactionInput
  -> Array TokenName
  -> Contract (Array (ScriptHash /\ TokenName))
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
  :: TransactionInput -> TokenName -> Contract (CurrencySymbol /\ TokenName)
mintNft txi tk = do
  cs /\ constraints /\ lookups <- mintNftConstraints txi tk
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ cs /\ tk

mkNftMintingPolicy
  :: TransactionInput -> TokenName -> Contract ScriptLookups
mkNftMintingPolicy txin tk = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope adminNftMintingPolicy
    plutusScriptFromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ [ toData txin, toData tk ]
  pure $ plutusMintingPolicy appliedScript

