module Test.CardanoRacers.Nft (adminNftSuite) where

import Contract.Prelude

import CardanoRacers.Nft (mkNftMintingPolicy) as Nft
import CardanoRacers.Nitro.Helpers (mintAdminNft) as NitroHelpers
import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Address (Address, getWalletAddresses)
import Contract.Monad (Contract, liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups
  ( ScriptLookups
  , mintingPolicy
  , mkUnbalancedTx
  , unspentOutputs
  ) as Lookups
import Contract.Scripts (applyArgs)
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Utils
  ( ContractAssertionFailure(UnexpectedTokenDelta)
  , ContractWrapAssertion
  , ExpectedActual(ExpectedActual)
  , Labeled
  , assertContract
  , checkBalanceDeltaAtAddress
  , label
  , runContractAssertionM
  , withAssertions
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (balanceTx)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName)
import Contract.Value (mkTokenName, scriptCurrencySymbol, singleton, valueOf) as Value
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable)
import Mote (group, test)
import Test.Spec.Assertions (shouldSatisfy)

adminNftSuite :: TestPlanM PlutipTest Unit
adminNftSuite = group "AdminNft" do
  test "Apply TxOutRef to script" do
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" $ getWalletUtxos
        txi /\ _ <- liftContractM "Could not find some utxo" $ Array.head $
          toUnfoldable utxos
        v2script <- liftContractM "Error decoding alwaysSucceeds" do
          envelope <- decodeTextEnvelope adminNftMintingPolicy
          plutusScriptV2FromEnvelope envelope
        let appliedScriptE = applyArgs v2script $ [ toData txi ]
        shouldSatisfy appliedScriptE isRight
  test "Mints NFT" do
    let
      assertNftMint
        :: forall (r :: Row Type)
         . Labeled Address
        -> ContractWrapAssertion r (CurrencySymbol /\ TokenName)
      assertNftMint addr contract =
        runContractAssertionM contract $
          checkBalanceDeltaAtAddress addr contract
            \nftAssetClass valueBefore valueAfter -> do
              let
                cs /\ tn = nftAssetClass

                actual :: BigInt
                actual =
                  Value.valueOf valueAfter cs tn - Value.valueOf valueBefore cs
                    tn

                expected :: BigInt
                expected = BigInt.fromInt 1

                unexpectedTokenDelta :: ContractAssertionFailure
                unexpectedTokenDelta =
                  UnexpectedTokenDelta addr tn (ExpectedActual expected actual)

              assertContract unexpectedTokenDelta (expected == actual)
              pure nftAssetClass

      withAssertionsMono
        :: forall (r :: Row Type)
         . ContractWrapAssertion r (CurrencySymbol /\ TokenName)
        -> Contract r (CurrencySymbol /\ TokenName)
        -> Contract r (CurrencySymbol /\ TokenName)
      withAssertionsMono = withAssertions
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        addr <- liftedM "Could not get wallet addresses" $ map Array.head
          getWalletAddresses
        txi /\ _ <- liftedM "Could not find some utxo"
          $ ((_ >>= Array.head) <<< map toUnfoldable)
          <$> getWalletUtxos
        void $ withAssertionsMono (assertNftMint $ label addr "Receiver") $
          NitroHelpers.mintAdminNft txi
  test "NFT minting policy fails to mint more than 1 token" $
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
        txi /\ _ <- liftContractM "Could not find some utxo"
          $ (Array.head <<< toUnfoldable)
          $ utxos
        tkname <- liftContractM "Cannot make token name"
          <<< (Value.mkTokenName <=< byteArrayFromAscii)
          $ "Token"
        policy <- Nft.mkNftMintingPolicy txi tkname
        cs <- liftContractM "couldn't get currency symbol" $
          Value.scriptCurrencySymbol policy
        let
          constraints :: Constraints.TxConstraints Void Void
          constraints =
            Constraints.mustMintValue
              (Value.singleton cs tkname $ BigInt.fromInt 2)
              <> Constraints.mustSpendPubKeyOutput txi

          lookups :: Lookups.ScriptLookups Void
          lookups =
            Lookups.mintingPolicy policy
              <> Lookups.unspentOutputs utxos

        unBalTx <- liftedE $ Lookups.mkUnbalancedTx lookups constraints
        res <- balanceTx unBalTx
        res `shouldSatisfy` isLeft
  where
  singleWalletDistribution :: InitialUTxOs
  singleWalletDistribution =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]
