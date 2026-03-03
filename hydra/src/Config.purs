module CardanoRacers.Hydra.Config
  ( AppConfig
  , PeerExtraConfig
  , appConfigCodec
  , configFromArgv
  ) where

import Prelude

import Cardano.Provider (ServerConfig)
import CardanoRacers.Hydra.Codec (serverConfigCodec)
import Contract.Config (LogLevel)
import Data.Codec.Argonaut (JPropCodec, JsonCodec, boolean, object, printJsonDecodeError) as CA
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Either (either)
import Effect (Effect)
import Effect.Exception (throw)
import HydraSdk.Lib (caDecodeFile, logLevelCodec)
import HydraSdk.Process (HydraNodeStartupParams, hydraNodeStartupParamsCodec)
import HydraSdk.Types (portCodec)
import Node.Process (argv)
import URI (Port)

type AppConfig =
  { hydraNodeStartupParams :: HydraNodeStartupParams PeerExtraConfig
  , serverPort :: Port
  , logLevel :: LogLevel
  , isHeadLeader :: Boolean
  }

appConfigCodec :: CA.JsonCodec AppConfig
appConfigCodec =
  CA.object "AppConfig" $ CAR.record
    { hydraNodeStartupParams: hydraNodeStartupParamsCodec peerExtraCodec
    , serverPort: portCodec
    , logLevel: logLevelCodec
    , isHeadLeader: CA.boolean
    }

type PeerExtraConfig =
  ( httpServer :: ServerConfig
  )

peerExtraCodec :: CA.JPropCodec (Record PeerExtraConfig)
peerExtraCodec = CAR.record
  { httpServer: serverConfigCodec
  }

configFromArgv :: Effect AppConfig
configFromArgv =
  argv >>= case _ of
    [ _, _, configFp ] ->
      either (throw <<< append "configFromArgv: " <<< CA.printJsonDecodeError) pure
        =<< caDecodeFile appConfigCodec configFp
    _ ->
      throw "configFromArgv: Unexpected number of command-line arguments."
