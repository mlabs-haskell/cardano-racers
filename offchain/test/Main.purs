-- | This module implements a test suite that uses Testnet to automate running
-- | contracts in temporary, private networks.
module Test.CardanoRacers.Main (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Test (ContractTest)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Testnet (Era(Conway), TestnetConfig, testTestnetContracts)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Data.Time.Duration (Seconds(Seconds))
import Data.UInt (fromInt) as UInt
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Test.CardanoRacers.AssetRequest (suite) as AssetRequest
import Test.CardanoRacers.Deposit (suite) as Deposit
import Test.CardanoRacers.Nft (suite) as Nft
import Test.CardanoRacers.Nitro.Contract (suite) as Nitro
import Test.CardanoRacers.Race (suite) as Race
import Test.CardanoRacers.RaceRegistry (suite) as RaceRegistry
import Test.CardanoRacers.RacersState.Contract (suite) as RacersState
import Test.Spec.Runner (defaultConfig)

-- Run with `npm run test`
main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig defaultConfig
      { timeout = Just $ Milliseconds 300_000.0, exit = true } $
      testTestnetContracts config suite

suite :: TestPlanM ContractTest Unit
suite = do
  Race.suite
  Nft.suite
  Nitro.suite
  RacersState.suite
  AssetRequest.suite
  Deposit.suite
  RaceRegistry.suite

config :: TestnetConfig
config =
  { logLevel: Info
  -- Server configs are used to deploy the corresponding services:
  , ogmiosConfig:
      { port: UInt.fromInt 1338
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
  , customLogger: Nothing
  , suppressLogs: true
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05
      , epochSize: Nothing
      , era: Conway
      , testnetMagic: 2
      }
  }

