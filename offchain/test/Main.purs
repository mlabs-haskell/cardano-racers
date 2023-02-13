-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.CardanoRacers.Main (main) where

import Contract.Prelude

import CardanoRacers.AdminNft (mintNft, mkNftMintingPolicy) as AdminNft
import CardanoRacers.Nitro.Contract
  ( buyNitroContract
  , initNitroStateContract
  , mintNitroContract
  , modifyNitroStateContract
  ) as NitroMint
import CardanoRacers.Nitro.Contract (mkNitroPolicy, queryNitroPolicyState)
import CardanoRacers.Nitro.Types
  ( NitroScriptParams(NitroScriptParams)
  , NitroState(NitroState)
  )
import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Address (Address, getWalletAddresses)
import Contract.Config (emptyHooks)
import Contract.Log (logInfo')
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
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip
  ( InitialUTxOs
  , PlutipConfig
  , PlutipTest
  , testPlutipContracts
  , withKeyWallet
  , withWallets
  )
import Contract.Test.Utils
  ( ContractAssertionFailure(UnexpectedTokenDelta)
  , ContractWrapAssertion
  , ExpectedActual(ExpectedActual)
  , Labeled
  , assertContract
  , assertTokenGainAtAddress
  , checkBalanceDeltaAtAddress
  , exitCode
  , interruptOnSignal
  , label
  , runContractAssertionM
  , withAssertions
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Transaction (balanceTx)
import Contract.TxConstraints as Constraints
import Contract.Utxos (getWalletBalance, getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, scriptCurrencySymbol)
import Contract.Value (mkTokenName, scriptCurrencySymbol, singleton, valueOf) as Value
import Contract.Wallet (KeyWallet)
import Data.Array (head) as Array
import Data.BigInt (BigInt)
import Data.BigInt (fromInt) as BigInt
import Data.Map (toUnfoldable)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Mote (group, only, test)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig defaultConfig
      { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

suite :: TestPlanM PlutipTest Unit
suite = do
  adminNftSuite
  only nitroTokenSuite

nitroTokenSuite :: TestPlanM PlutipTest Unit
nitroTokenSuite = group "NitroToken script" do
  test "Admin mints Nitro" do
    withWallets walletUtxoDistr \w ->
      withKeyWallet w $ do
        nitroTk <- liftContractM "Cannot make token name"
          <<< (Value.mkTokenName <=< byteArrayFromAscii)
          $ "Nitro"
        ownAddress <- liftedM "Couldn't get wallet address" $ Array.head <$>
          getWalletAddresses
        (csAdmin /\ tkAdmin) <- mintNftAuto "Admin"
        (csState /\ tkState) <- mintNftAuto "State"
        let
          nsp = NitroScriptParams
            { adminToken: csAdmin /\ tkAdmin
            , stateToken: csState /\ tkState
            , nitroToken: nitroTk
            }
          amountToMint = BigInt.fromInt 100
        nitroSymbol <-
          liftedM "Couldn't create currency symbol from NitroPolicy"
            $ scriptCurrencySymbol
            <$> mkNitroPolicy nsp
        let
          withAssertionsMono
            :: forall (r :: Row Type)
             . Array (ContractWrapAssertion r Unit)
            -> Contract r Unit
            -> Contract r Unit
          withAssertionsMono = withAssertions
        withAssertionsMono
          [ assertTokenGainAtAddress (label ownAddress "Admin")
              (nitroSymbol /\ nitroTk)
              (const $ pure amountToMint)
          ] $ NitroMint.mintNitroContract nsp amountToMint

  test "Initialises NitroState" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) -> do
      let nitroPrice = BigInt.fromInt 1000000
      adminAddr <- withKeyWallet admin $ liftedM "Could not get admin address"
        $ Array.head
        <$> getWalletAddresses
      treasuryAddr <- withKeyWallet treasury
        $ liftedM "Could not get treasury address"
        $ Array.head
        <$> getWalletAddresses
      nsp <- initNitroPolicyWithWallets (admin /\ treasury) nitroPrice
      let
        expectedNitroState = NitroState
          { nitroPrice: nitroPrice
          , treasuryAddress: treasuryAddr
          , operatingAddress: adminAddr
          }
      (onchainNitroState /\ _) <- queryNitroPolicyState nsp
      onchainNitroState `shouldEqual` expectedNitroState
  test "Modifies NitroState" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr) \(admin /\ treasury) -> do
      nsp <- initNitroPolicyWithWallets (admin /\ treasury) $ BigInt.fromInt
        1000000
      withKeyWallet admin do
        addr <- liftedM "Could not get address" $ Array.head <$>
          getWalletAddresses
        let
          nitroState = NitroState
            { nitroPrice: BigInt.fromInt 2000000
            , treasuryAddress: addr
            , operatingAddress: addr
            }
        NitroMint.modifyNitroStateContract nsp nitroState
        (updatedNitroState /\ _) <- queryNitroPolicyState nsp
        nitroState `shouldEqual` updatedNitroState
  only $ test "User buys Nitro" do
    withWallets (walletUtxoDistr /\ walletUtxoDistr /\ walletUtxoDistr)
      \(admin /\ treasury /\ bob) -> do
        nsp <- initNitroPolicyWithWallets (admin /\ treasury) $ BigInt.fromInt
          1000000
        withKeyWallet bob do
          NitroMint.buyNitroContract nsp $ BigInt.fromInt 100
          getWalletBalance >>= logInfo' <<< show
  where
  walletUtxoDistr :: InitialUTxOs
  walletUtxoDistr =
    [ BigInt.fromInt 5_000_000
    , BigInt.fromInt 2_000_000_000
    ]

  mintNftAuto :: String -> Contract () (CurrencySymbol /\ TokenName)
  mintNftAuto tkstring = do
    tkName <- liftContractM "Cannot make token name"
      <<< (Value.mkTokenName <=< byteArrayFromAscii)
      $ tkstring
    utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
    (txi /\ _) <- liftContractM "Could not find some utxo"
      $ (Array.head <<< toUnfoldable)
      $ utxos
    res <- AdminNft.mintNft txi tkName
    logInfo' "Minted NFT successfully"
    pure res

  initNitroPolicyWithWallets
    :: (KeyWallet /\ KeyWallet) -> BigInt -> Contract () NitroScriptParams
  initNitroPolicyWithWallets (admin /\ treasury) nitroPrice = do
    treasuryAddr <- withKeyWallet treasury
      $ liftedM "Could not get address"
      $ Array.head
      <$> getWalletAddresses
    withKeyWallet admin do
      nitroTk <- liftContractM "Cannot make token name"
        <<< (Value.mkTokenName <=< byteArrayFromAscii)
        $ "Nitro"
      (csAdmin /\ tkAdmin) <- mintNftAuto "Admin"
      (csState /\ tkState) <- mintNftAuto "State"
      ownAddr <- liftedM "Could not get address" $ Array.head <$>
        getWalletAddresses
      let
        nsp = NitroScriptParams
          { adminToken: csAdmin /\ tkAdmin
          , stateToken: csState /\ tkState
          , nitroToken: nitroTk
          }
        ns = NitroState
          { nitroPrice: nitroPrice
          , treasuryAddress: treasuryAddr
          , operatingAddress: ownAddr
          }
      NitroMint.initNitroStateContract nsp ns
      pure nsp

adminNftSuite :: TestPlanM PlutipTest Unit
adminNftSuite = group "AdminNft" do
  test "apply TxOutRef to script" do
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" $ getWalletUtxos
        (txi /\ _) <- liftContractM "Could not find some utxo" $ Array.head $
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
                (cs /\ tn) = nftAssetClass

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
        (txi /\ _) <- liftedM "Could not find some utxo"
          $ ((_ >>= Array.head) <<< map toUnfoldable)
          <$> getWalletUtxos
        tkname <- liftContractM "Cannot make token name"
          <<< (Value.mkTokenName <=< byteArrayFromAscii)
          $ "CardanoRacersAdminNFT"
        void $ withAssertionsMono (assertNftMint $ label addr "Receiver") $
          AdminNft.mintNft txi tkname
  test "NFT minting policy fails to mint more than 1 token" $
    withWallets singleWalletDistribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" getWalletUtxos
        (txi /\ _) <- liftContractM "Could not find some utxo"
          $ (Array.head <<< toUnfoldable)
          $ utxos
        tkname <- liftContractM "Cannot make token name"
          <<< (Value.mkTokenName <=< byteArrayFromAscii)
          $ "Token"
        policy <- AdminNft.mkNftMintingPolicy txi
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

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Trace
  , ogmiosConfig:
      { port: UInt.fromInt 1338
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , ogmiosDatumCacheConfig:
      { port: UInt.fromInt 10000
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , kupoConfig:
      { port: UInt.fromInt 1443
      , host: "127.0.0.1"
      , secure: false
      , path: Nothing
      }
  , postgresConfig:
      { host: "127.0.0.1"
      , port: UInt.fromInt 5433
      , user: "ctxlib"
      , password: "ctxlib"
      , dbname: "ctxlib"
      }
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }
