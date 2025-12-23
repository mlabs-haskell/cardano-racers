module CardanoRacers.Hydra.Server
  ( cleanupHttpServer
  , httpServer
  ) where

import Prelude

import CardanoRacers.Hydra.Handlers.HostRace (hostRaceHandler)
import CardanoRacers.Hydra.Handlers.SignCommitTx (signCommitTxHandler)
import CardanoRacers.Hydra.Monad (AppLogger, AppM, AppState, runApp)
import Data.Newtype (unwrap)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (liftAff)
import Effect.Console (log)
import HTTPure
  ( Headers
  , Request
  , Response
  , ServerM
  , emptyResponse'
  , header
  , headers
  , notFound
  , serve
  , toString
  ) as HTTPure
import HTTPure (Method(Post, Options), (!?), (!@))
import HTTPure.Status (ok) as HTTPureStatus
import URI.Port (toInt) as Port

httpServer :: AppState -> AppLogger -> HTTPure.ServerM
httpServer state logger = do
  let port = Port.toInt state.config.serverPort
  HTTPure.serve port (router state logger) do
    log $ "Http server now accepts connections on port " <> show port <> "."

router :: AppState -> AppLogger -> HTTPure.Request -> Aff HTTPure.Response
router _ _ { method: Options, headers }
  | unwrap headers !? "Access-Control-Request-Method" =
      corsPreflightHandler headers
  | otherwise =
      HTTPure.notFound

router state logger request =
  corsMiddleware (runApp state logger <<< routerCors) request

routerCors :: HTTPure.Request -> AppM HTTPure.Response
routerCors { body, method: Post, path: [ "hostRace" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  hostRaceHandler bodyStr

routerCors { body, method: Post, path: [ "signCommitTx" ] } = do
  bodyStr <- liftAff $ HTTPure.toString body
  signCommitTxHandler bodyStr

routerCors _ = HTTPure.notFound

corsMiddleware
  :: (HTTPure.Request -> Aff HTTPure.Response)
  -> HTTPure.Request
  -> Aff HTTPure.Response
corsMiddleware router' request =
  router' request <#> \response ->
    response
      { headers =
          response.headers <>
            HTTPure.header "Access-Control-Allow-Origin" "*"
      }

corsPreflightHandler :: HTTPure.Headers -> Aff HTTPure.Response
corsPreflightHandler headers =
  HTTPure.emptyResponse' HTTPureStatus.ok $
    HTTPure.headers
      [ "Access-Control-Allow-Origin" /\ "*"
      , "Access-Control-Allow-Methods" /\ (headers !@ "Access-Control-Request-Method")
      , "Access-Control-Allow-Headers" /\ (headers !@ "Access-Control-Request-Headers")
      ]

cleanupHttpServer :: Effect Unit -> Effect Unit
cleanupHttpServer runCleanup = do
  log "Stopping HTTP server."
  runCleanup
