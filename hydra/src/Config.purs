module CardanoRacers.Hydra.Config
  ( AppConfig
  , appConfigCodec
  , configFromArgv
  ) where

import Prelude

import Contract.Config (LogLevel)
import Data.Codec.Argonaut (JsonCodec, object, printJsonDecodeError) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (throw)
import HydraSdk.Lib (caDecodeFile, logLevelCodec)
import HydraSdk.Process (HydraNodeStartupParams, hydraNodeStartupParamsCodec)
import Node.Process (argv)

type AppConfig =
  { hydraNodeStartupParams :: HydraNodeStartupParams
  , logLevel :: LogLevel
  }

appConfigCodec :: CA.JsonCodec AppConfig
appConfigCodec =
  CA.object "AppConfig" $ CAR.record
    { hydraNodeStartupParams: hydraNodeStartupParamsCodec
    , logLevel: logLevelCodec
    }

configFromArgv :: Effect AppConfig
configFromArgv =
  argv >>= case _ of
    [ _, _, configFp ] ->
      either (throw <<< append "configFromArgv: " <<< CA.printJsonDecodeError) pure
        =<< caDecodeFile appConfigCodec configFp
    _ ->
      throw "configFromArgv: Unexpected number of command-line arguments."
