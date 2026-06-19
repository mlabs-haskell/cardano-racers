module CardanoRacers.Hydra.Main
  ( main
  , cleanupHandler
  ) where

import Prelude

import CardanoRacers.Hydra.Config (configFromArgv)
import CardanoRacers.Hydra.Monad (AppState, appLogger, cleanupApp, initApp, launchApp, runApp)
import CardanoRacers.Hydra.Node (HydraNodeHandle, cleanupHydraNode, startHydraNode)
import CardanoRacers.Hydra.Server (cleanupHttpServer, httpServer)
import Contract.Log (logError')
import Data.Maybe (maybe)
import Data.Posix.Signal (Signal(SIGINT, SIGTERM))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Exception (message, name, stack) as Error
import Node.Process (onSignal, onUncaughtException)

main :: Effect Unit
main =
  launchAff_ do
    config <- liftEffect configFromArgv
    state <- initApp config
    let logger = appLogger
    hydraNodeHandle <- runApp state logger startHydraNode
    closeHttpServer <- liftEffect $ httpServer state logger
    let runCleanup = cleanupHandler state hydraNodeHandle closeHttpServer
    liftEffect do
      onUncaughtException \err -> do
        launchApp state logger $ logError' $
          "UNCAUGHT "
            <> Error.name err
            <> ": "
            <> Error.message err
            <> maybe mempty (append ", STACK: ") (Error.stack err)
        runCleanup
      onSignal SIGINT runCleanup
      onSignal SIGTERM runCleanup

cleanupHandler :: AppState -> HydraNodeHandle -> (Effect Unit -> Effect Unit) -> Effect Unit
cleanupHandler appState hydraNodeHandle closeHttpServer = do
  cleanupHttpServer $ closeHttpServer $ pure unit
  cleanupHydraNode hydraNodeHandle
  cleanupApp appState
