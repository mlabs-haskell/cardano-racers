-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.Scaffold.Main (main) where

import Contract.Prelude

import AdminNft (contract) as AdminNft
import CardanoRacers.ScriptsFFI (adminNftMintingPolicy)
import Contract.Address (Address, getWalletAddresses)
import Contract.Config (emptyHooks)
import Contract.Monad (Contract, liftContractM, liftedM)
import Contract.PlutusData (toData)
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
  , checkBalanceDeltaAtAddress
  , exitCode
  , interruptOnSignal
  , label
  , runContractAssertionM
  , withAssertions
  )
import Contract.TextEnvelope (decodeTextEnvelope, plutusScriptV2FromEnvelope)
import Contract.Utxos (getWalletUtxos)
import Contract.Value (CurrencySymbol, TokenName, valueOf)
import Data.Array (head)
import Data.BigInt (BigInt, fromInt)
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
import Mote (test)
import Scaffold (contract) as Scaffold
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 70_000.0, exit = true } $
      testPlutipContracts config suite

suite :: TestPlanM PlutipTest Unit
suite = do
  test "Print PubKey" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 2_000_000_000
        ]
    withWallets distribution \w ->
      withKeyWallet w do
        Scaffold.contract

  test "NFTScript apply TxOutRef" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 2_000_000_000
        ]
    withWallets distribution \w ->
      withKeyWallet w do
        utxos <- liftedM "Could not get wallet utxos" $ getWalletUtxos
        (txi /\ _) <- liftContractM "Could not find some utxo" $ head $
          toUnfoldable utxos
        v2script <- liftContractM "Error decoding alwaysSucceeds" do
          envelope <- decodeTextEnvelope adminNftMintingPolicy
          plutusScriptV2FromEnvelope envelope
        let appliedScriptE = applyArgs v2script $ [ toData txi ]
        shouldSatisfy appliedScriptE isRight
  test "Mints NFT" do
    let
      distribution :: InitialUTxOs
      distribution =
        [ BigInt.fromInt 5_000_000
        , BigInt.fromInt 2_000_000_000
        ]

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
                  valueOf valueAfter cs tn - valueOf valueBefore cs tn

                expected :: BigInt
                expected = fromInt 1

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
    withWallets distribution \w ->
      withKeyWallet w do
        addr <- liftedM "Could not get wallet addresses" $ map head
          getWalletAddresses
        void $ withAssertionsMono (assertNftMint $ label addr "Receiver")
          AdminNft.contract
        pure unit

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
