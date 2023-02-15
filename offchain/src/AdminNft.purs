-- | This module contains a contract that mints an nft using a utxo from user
-- | wallet and CardanoRacersAdminNFT as the token name
module CardanoRacers.AdminNft
  ( mintAdminNft
  , mintStateNft
  , mintAdminAndStateNfts
  , mkNftMintingPolicy
  ) where

import Contract.Prelude

import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM, wrapContract)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups as Lookups
import Contract.Scripts (MintingPolicy(PlutusMintingPolicy), applyArgs)
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

mintNftConstraints
  :: TransactionInput
  -> TokenName
  -> Contract ()
       ( CurrencySymbol /\ (Constraints.TxConstraints Void Void) /\
           (Lookups.ScriptLookups Void)
       )
mintNftConstraints txi tkname = do
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

  pure $ cs /\ constraints /\ lookups

type AssetClass = CurrencySymbol /\ TokenName

mintAdminAndStateNfts
  :: (TransactionInput /\ TransactionInput)
  -> Contract () (AssetClass /\ AssetClass)
mintAdminAndStateNfts (txiAdmin /\ txiState) = do
  adminTk <-
    liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersAdmin"
  adminCs /\ adminConstraints /\ adminLookups <- mintNftConstraints txiAdmin
    adminTk

  stateTk <-
    liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersNitroState"
  stateCs /\ stateConstraints /\ stateLookups <- mintNftConstraints txiState
    stateTk

  let
    constraints = adminConstraints <> stateConstraints
    lookups = adminLookups <> stateLookups

    adminAsset = adminCs /\ adminTk
    stateAsset = stateCs /\ stateTk

  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ (adminAsset /\ stateAsset)

mintNft
  :: TransactionInput -> TokenName -> Contract () (CurrencySymbol /\ TokenName)
mintNft txi tk = do
  cs /\ constraints /\ lookups <- mintNftConstraints txi tk
  txId <- submitTxFromConstraints lookups constraints
  awaitTxConfirmed txId
  pure $ cs /\ tk

mintAdminNft :: TransactionInput -> Contract () (CurrencySymbol /\ TokenName)
mintAdminNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersAdmin"
  )

mintStateNft :: TransactionInput -> Contract () (CurrencySymbol /\ TokenName)
mintStateNft txi = mintNft txi =<<
  ( liftContractM "Cannot make token name"
      <<< (mkTokenName <=< byteArrayFromAscii)
      $ "RacersNitroState"
  )

mkNftMintingPolicy :: TransactionInput -> Contract () MintingPolicy
mkNftMintingPolicy txin = do
  v2script <- liftContractM "Error decoding alwaysSucceeds" do
    envelope <- decodeTextEnvelope adminNftMintingPolicy
    plutusScriptV2FromEnvelope envelope
  appliedScript <- liftEither $ left (error <<< show) $ applyArgs v2script
    $ Array.singleton
    $ toData txin
  pure $ PlutusMintingPolicy appliedScript

