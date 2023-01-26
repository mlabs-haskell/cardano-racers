-- | This module contains a contract that mints an nft using a utxo from user
-- | wallet and CardanoRacersAdminNFT as the token name
module AdminNft (contract) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Log (logInfo')
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(..), applyArgs)
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (TransactionInput, awaitTxConfirmed, submitTxFromConstraints)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value (mkTokenName, scriptCurrencySymbol)
import Contract.Value (singleton) as Value
import Control.Monad.Error.Class (liftMaybe)
import Data.Array (head, singleton) as Array
import Data.Map (toUnfoldable)
import Data.Profunctor.Choice (left)
import Data.Tuple.Nested ((/\))
import Effect.Exception (error)

contract :: Contract () Unit
contract = do
  utxos <- liftedM "Could not get wallet utxos" $ getWalletUtxos

  tkname <- liftContractM "Couldn't convert to hex" $ (mkTokenName <=< byteArrayFromAscii) "CardanoRacersAdminNFT"
  (txi /\ _) <- liftContractM "Could not find some utxo" $ Array.head $ toUnfoldable utxos

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
        <> Lookups.unspentOutputs utxos

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  logInfo' "Tx submitted successfully!"

mkNftMintingPolicy :: TransactionInput -> Contract () MintingPolicy
mkNftMintingPolicy txin = do
  v2script <- liftMaybe (error "Error decoding alwaysSucceeds") do
    envelope <- decodeTextEnvelope adminNftMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script $ Array.singleton $ toData txin
  pure $ PlutusMintingPolicy appliedScript

