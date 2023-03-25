module Test.CardanoRacers.Unit (main) where

import Contract.Prelude

import Contract.Test.Mote (TestPlanM, interpretWithConfig)
import Contract.Test.Utils (exitCode, interruptOnSignal)
import Data.Posix.Signal (Signal(SIGINT))
import Effect.Aff
  ( Milliseconds(Milliseconds)
  , cancelWith
  , effectCanceler
  , launchAff
  )
import Test.CardanoRacers.GameAsset.Parameters as GameAssetParameters
import Test.CardanoRacers.Nitro.Types as NitroTypes
import Test.Spec.Runner (defaultConfig)

main :: Effect Unit
main = interruptOnSignal SIGINT =<< launchAff do
  flip cancelWith (effectCanceler (exitCode 1)) do
    interpretWithConfig
      defaultConfig { timeout = Just $ Milliseconds 30_000.0, exit = true }
      testPlan

testPlan :: TestPlanM (Aff Unit) Unit
testPlan = do
  NitroTypes.suite
  GameAssetParameters.suite

