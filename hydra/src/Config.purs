module CardanoRacers.Hydra.Config
  ( AppConfig
  , AppQueryBackend(Blockfrost, Kupmios)
  , AppTimeParams
  , DevParams
  , PeerExtraConfig
  , appConfigCodec
  , configFromArgv
  ) where

import Prelude

import Cardano.Provider (ServerConfig)
import Cardano.Types (NetworkId)
import CardanoRacers.Hydra.Codec (networkIdCodec, serverConfigCodec)
import Contract.Config (LogLevel)
import Data.Codec.Argonaut
  ( JPropCodec
  , JsonCodec
  , boolean
  , int
  , object
  , printJsonDecodeError
  , string
  ) as CA
import Data.Codec.Argonaut.Record (optional, record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Exception (throw)
import HydraSdk.Lib (caDecodeFile, logLevelCodec)
import HydraSdk.Process (HydraNodeStartupParams, hydraNodeStartupParamsCodec)
import HydraSdk.Types (portCodec)
import Node.Path (FilePath)
import Node.Process (argv)
import URI (Port)

type AppConfig =
  { hydraNodeStartupParams :: HydraNodeStartupParams PeerExtraConfig
  , queryBackend :: AppQueryBackend
  , serverPort :: Port
  , logLevel :: LogLevel
  , isHeadLeader :: Boolean
  , timeParams :: AppTimeParams
  , devParams :: Maybe DevParams
  }

appConfigCodec :: CA.JsonCodec AppConfig
appConfigCodec =
  CA.object "AppConfig" $ CAR.record
    { hydraNodeStartupParams: hydraNodeStartupParamsCodec peerExtraCodec
    , queryBackend: appQueryBackendCodec
    , serverPort: portCodec
    , logLevel: logLevelCodec
    , isHeadLeader: CA.boolean
    , timeParams: appTimeParamsCodec
    , devParams: CAR.optional devParamsCodec
    }

type DevParams =
  { mockRaceSimulator :: Boolean
  }

devParamsCodec :: CA.JsonCodec DevParams
devParamsCodec =
  CA.object "DevParams" $ CAR.record
    { mockRaceSimulator: CA.boolean
    }

type AppTimeParams =
  { playerInputSubmitWindowSec :: Int
  }

appTimeParamsCodec :: CA.JsonCodec AppTimeParams
appTimeParamsCodec =
  CA.object "AppTimeParams" $ CAR.record
    { playerInputSubmitWindowSec: CA.int
    }

data AppQueryBackend
  = Blockfrost { apiKeyFile :: FilePath }
  | Kupmios
      { network :: NetworkId
      , kupoConfig :: ServerConfig
      , ogmiosConfig :: ServerConfig
      }

derive instance Generic AppQueryBackend _
derive instance Eq AppQueryBackend

instance Show AppQueryBackend where
  show = genericShow

appQueryBackendCodec :: CA.JsonCodec AppQueryBackend
appQueryBackendCodec =
  CAS.sumFlat "AppQueryBackend"
    { "Blockfrost":
        CAR.record
          { apiKeyFile: CA.string
          }
    , "Kupmios":
        CAR.record
          { network: networkIdCodec
          , kupoConfig: serverConfigCodec
          , ogmiosConfig: serverConfigCodec
          }
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
