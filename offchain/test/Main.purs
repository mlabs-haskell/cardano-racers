-- | This module implements a test suite that uses Plutip to automate running
-- | contracts in temporary, private networks.
module Test.CardanoRacers.Main (main) where

import Contract.Prelude

import Contract.Config (emptyHooks)
import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Plutip (PlutipConfig, PlutipTest, testPlutipContracts)
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
import Mote (only)
import Test.CardanoRacers.AssetRequest (suite) as AssetRequest
import Test.CardanoRacers.Deposit (suite) as Deposit
import Test.CardanoRacers.Nft (suite) as Nft
import Test.CardanoRacers.Nitro.Contract (suite) as Nitro
import Test.CardanoRacers.RaceRegistry (suite) as RaceRegistry
import Test.CardanoRacers.RacersState.Contract (suite) as RacersState
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
  Nft.suite
  only Nitro.suite
  RacersState.suite
  AssetRequest.suite
  Deposit.suite
  GameAsset.suite
  only RaceRegistry.suite

config :: PlutipConfig
config =
  { host: "127.0.0.1"
  , port: UInt.fromInt 8082
  , logLevel: Info
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
  , suppressLogs: false
  , hooks: emptyHooks
  , clusterConfig:
      { slotLength: Seconds 0.05 }
  }
