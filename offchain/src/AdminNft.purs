-- | This module contains a contract that mints an nft using a utxo from user
-- | wallet and CardanoRacersAdminNFT as the token name
module AdminNft (mintNft, mkNftMintingPolicy) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , throwContractError
  , wrapContract
  )
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction
  ( TransactionInput
  , awaitTxConfirmed
  , submitTxFromConstraints
  )
import Contract.TxConstraints as Constraints
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , mkTokenName
  , scriptCurrencySymbol
  )
import Contract.Value (singleton) as Value
import Ctl.Internal.Plutus.Conversion (toPlutusTxOutputWithRefScript)
import Ctl.Internal.QueryM.Kupo (getUtxoByOref)
import Data.Array (singleton) as Array
import Data.Map (singleton)
import Data.Profunctor.Choice (left)
import Effect.Exception (error)

mintNft
  :: TransactionInput
  -> TokenName
  -> Contract () (Tuple CurrencySymbol TokenName)
mintNft txi tkname = do
  txo <- liftedM "Could not get utxos" $ liftedE $ wrapContract $ getUtxoByOref
    txi
  ptxo <- liftContractM "Could not convert to plutus txo" $
    toPlutusTxOutputWithRefScript txo

  mp <- mkNftMintingPolicy txi
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

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ cs /\ tkname

mkNftMintingPolicy :: TransactionInput -> Contract () MintingPolicy
mkNftMintingPolicy txin = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope adminNftMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData txin
  pure $ PlutusMintingPolicy appliedScript

