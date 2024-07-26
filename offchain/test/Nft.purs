module Test.CardanoRacers.Nft (suite) where

import Contract.Prelude

import Cardano.Plutus.ApplyArgs (applyArgs)
import Cardano.Plutus.Types.TokenName (TokenName(..), mkTokenName)
import Cardano.Types.Asset (Asset(..))
import Cardano.Types.BigInt as BigInt
import Cardano.Types.BigNum as BigNum
import Cardano.Types.Int (fromInt) as Int
import Cardano.Types.Mint (singleton) as Mint
import Cardano.Types.PlutusScript as PlutusScript
import Cardano.Types.ScriptHash (ScriptHash)
import Cardano.Types.Value (Value)
import Cardano.Types.Value (valueOf) as Value
import CardanoRacers.Nft (mkNftMintingPolicy) as Nft
import CardanoRacers.Nitro.Helpers (mintAdminNft) as NitroHelpers
import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Address (Address)
import Contract.Monad (liftContractM, liftedE, liftedM)
import Contract.PlutusData (toData)
import Contract.Prim.ByteArray (byteArrayFromAscii)
import Contract.ScriptLookups (ScriptLookups, unspentOutputs) as Lookups
import Contract.Test.Assert
  ( ContractAssertion
  , ContractAssertionFailure(UnexpectedTokenDelta)
  , ContractCheck
  , ExpectedActual(ExpectedActual)
  , Labeled
  , assertContract
  , checkValueDeltaAtAddress
  , label
  , runChecks
  )
import Contract.Test.Mote (TestPlanM)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipTest
  , withKeyWallet
  , withWallets
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptFromEnvelope)
import Contract.Transaction (balanceTxE)
import Contract.TxConstraints as Constraints
import Contract.UnbalancedTx as Contract
import Contract.Wallet (getWalletAddresses, getWalletUtxos)
import Control.Monad.Trans.Class (lift)
import Data.Array (head)
import Data.Array (head) as Array
import Data.Map (toUnfoldable)
import Mote (group, test)
import Test.Spec.Assertions (shouldSatisfy)

suite :: TestPlanM PlutipTest Unit
suite = group "AdminNft" do
  test "Apply TxOutRef to script" do
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" $ getWalletUtxos
        txi /\ _ <- liftContractM "Could not find some utxo" $ Array.head $
          toUnfoldable utxos
        v2script <- liftContractM "Error decoding alwaysSucceeds" do
          envelope <- decodeTextEnvelope adminNftMintingPolicy
          plutusScriptFromEnvelope envelope
        let appliedScriptE = applyArgs v2script [ toData txi ]
        shouldSatisfy appliedScriptE isRight
  test "Mints Admin NFT" do
    let
      checkNftGain
        :: forall (r :: Row Type)
         . Labeled Address
        -> ContractCheck (ScriptHash /\ TokenName)
      -- checkValueDeltaAtAddress expects Cardano.Types.Value for `check`
      checkNftGain addr contract = checkValueDeltaAtAddress addr check contract
        where
        check
          :: Maybe (ScriptHash /\ TokenName)
          -> Value
          -> Value
          -> ContractAssertion Unit
        check result valueBefore valueAfter = do
          (cs /\ tn) <- lift $ liftContractM
            "Could not get contract result (CurrencySymbol,TokenName)"
            result
          let
            asset = Asset cs (unwrap tn)
            actual = BigNum.toBigInt (Value.valueOf asset valueAfter) -
              BigNum.toBigInt (Value.valueOf asset valueBefore)

            expected = BigInt.fromInt 1

            unexpectedTokenDelta :: ContractAssertionFailure
            unexpectedTokenDelta =
              UnexpectedTokenDelta (Just addr) (unwrap tn)
                (ExpectedActual expected actual)

          assertContract unexpectedTokenDelta (actual == expected)
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        addr <- liftedM "Could not get wallet addresses" $ map Array.head
          getWalletAddresses
        txi /\ _ <- liftedM "Could not find some utxo"
          $ ((_ >>= Array.head) <<< map toUnfoldable)
          <$> getWalletUtxos
        void $ runChecks [ checkNftGain $ label addr "Receiver" ] $ lift
          $ ((map TokenName) <$> NitroHelpers.mintAdminNft txi)

  test "NFT minting policy fails to mint more than 1 token" $
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
        txi /\ _ <- liftContractM "Could not find some utxo"
          $ (Array.head <<< toUnfoldable) utxos
        tkname <- liftContractM "Cannot make token name"
          <<< (mkTokenName <=< byteArrayFromAscii)
          $ "Token"
        policy <- Nft.mkNftMintingPolicy txi (unwrap tkname)
        mpHash <-
          liftContractM "Could not get minting policy script hash"
            $ head
            $ map PlutusScript.hash
            $ (unwrap policy).plutusMintingPolicies
        let
          constraints :: Constraints.TxConstraints
          constraints =
            Constraints.mustMintValue
              (Mint.singleton mpHash (unwrap tkname) $ Int.fromInt 2)
              <> Constraints.mustSpendPubKeyOutput txi

          lookups :: Lookups.ScriptLookups
          lookups = policy <> Lookups.unspentOutputs utxos

        unBalTx <- liftedE $ Contract.mkUnbalancedTxE lookups constraints
        res <- balanceTxE unBalTx
        res `shouldSatisfy` isLeft
  where
  singleWalletDistribution :: InitialUTxOs
  singleWalletDistribution =
    [ BigNum.fromInt 5_000_000
    , BigNum.fromInt 2_000_000_000
    ]
