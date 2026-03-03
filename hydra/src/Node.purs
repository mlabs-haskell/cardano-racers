module CardanoRacers.Hydra.Node
  ( HydraNodeHandle
  , cleanupHydraNode
  , startHydraNode
  ) where

import Prelude

import CardanoRacers.Hydra.MessageHandler (messageHandler)
import CardanoRacers.Hydra.Monad (AppM, getAppLauncher, readHeadStatus, setHeadStatus)
import Contract.Log (logError', logTrace', logWarn')
import Control.Monad.Reader (ask)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Posix.Signal (Signal(SIGINT))
import Data.Traversable (traverse_)
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref)
import Effect.Ref (new, read, write) as Ref
import HydraSdk.NodeApi
  ( HydraTxRetryStrategy(RetryTxWithParams, DontRetryTx)
  , HydraNodeApiWebSocket
  , mkHydraNodeApiWebSocket
  )
import HydraSdk.Process (spawnHydraNode)
import HydraSdk.Types (HydraHeadStatus(HeadStatus_Closed), printHostPort)
import Node.ChildProcess (ChildProcess, kill)

type HydraNodeHandle =
  { hydraNodeProcess :: ChildProcess
  , hydraNodeApiWsRef :: Ref (Maybe (HydraNodeApiWebSocket AppM))
  }

startHydraNode :: AppM HydraNodeHandle
startHydraNode = do
  { config } <- ask
  appEff <- getAppLauncher
  wsRef <- liftEffect $ Ref.new Nothing
  hydraNodeProcess <- spawnHydraNode config.hydraNodeStartupParams
    { apiServerStartedHandler: Just $ appEff $ onApiServerStarted wsRef
    , stdoutHandler: Just (appEff <<< logTrace' <<< append "[hydra-node:stdout] ")
    , stderrHandler: Just (appEff <<< logWarn' <<< append "[hydra-node:stderr] ")
    }
  pure
    { hydraNodeProcess
    , hydraNodeApiWsRef: wsRef
    }

onApiServerStarted :: Ref (Maybe (HydraNodeApiWebSocket AppM)) -> AppM Unit
onApiServerStarted wsRef = do
  { config } <- ask
  runM <- getAppLauncher
  let url = "ws://" <> printHostPort config.hydraNodeStartupParams.hydraNodeApiAddress
  ws <- mkHydraNodeApiWebSocket
    { url
    , runM
    , handlers:
        { connectHandler: const (pure unit)
        , messageHandler
        , headStatusHandler: Just setHeadStatus
        , errorHandler: \_ws err -> logError' $ "hydra-node API WebSocket error: " <> show err
        }
    , txRetryStrategies:
        { close:
            RetryTxWithParams
              { delaySec: 90
              , maxRetries: top
              , successPredicate: (_ >= HeadStatus_Closed) <$> readHeadStatus
              , failHandler: pure unit
              }
        , contest: DontRetryTx
        }
    }
  liftEffect $ Ref.write (Just ws) wsRef

cleanupHydraNode :: HydraNodeHandle -> Effect Unit
cleanupHydraNode handle = do
  log "Stopping hydra-node."
  kill SIGINT handle.hydraNodeProcess
  log "Closing hydra-node API WebSocket connection."
  Ref.read handle.hydraNodeApiWsRef >>= traverse_ _.baseWs.close
