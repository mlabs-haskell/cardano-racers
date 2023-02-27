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
import Test.CardanoRacers.Nft (adminNftSuite)
import Test.CardanoRacers.Nitro.Contract (nitroTokenSuite)
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
  nitroTokenSuite

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
      { slotLength: Seconds 0.05 }
  }
