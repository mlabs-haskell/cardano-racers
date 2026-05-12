module CardanoRacers.Hydra.RaceSimulator
  ( RaceSimulationError
      ( NoResultFileAvailableAfterTimeout
      , ResultStringHasUnexpectedPrefix
      , CouldNotConvertResultToNumber
      )
  , raceSimulationErrorCodec
  , runSimulator
  ) where

import Prelude

import Contract.Log (logTrace')
import Control.Monad.Error.Class (class MonadError, throwError, try)
import Control.Monad.Logger.Class (class MonadLogger)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Testnet.Utils (tmpdirUnique)
import Data.Array ((:))
import Data.Array (concat) as Array
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Either (Either(Left), isRight, note)
import Data.Generic.Rep (class Generic)
import Data.Int (toNumber) as Int
import Data.Maybe (Maybe(Just, Nothing))
import Data.Number (fromString) as Number
import Data.Posix.Signal (Signal(SIGTERM))
import Data.Show.Generic (genericShow)
import Data.String (Pattern(Pattern))
import Data.String (stripPrefix, trim) as String
import Data.Time.Duration (Seconds(Seconds))
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Retry (constantDelay, limitRetriesByCumulativeDelay, recovering)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (Error, error)
import Node.ChildProcess (defaultSpawnOptions, kill, spawn, stdout)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Stream (onDataString)

data RaceSimulationError
  = NoResultFileAvailableAfterTimeout
  | ResultStringHasUnexpectedPrefix
  | CouldNotConvertResultToNumber

derive instance Generic RaceSimulationError _
derive instance Eq RaceSimulationError

instance Show RaceSimulationError where
  show = genericShow

raceSimulationErrorCodec :: CA.JsonCodec RaceSimulationError
raceSimulationErrorCodec = CAG.nullarySum "RaceSimulationError"

runSimulator
  :: forall (m :: Type -> Type)
   . MonadAff m
  => MonadError Error m
  => MonadLogger m
  => FilePath
  -> m (Either RaceSimulationError Number)
runSimulator inputCsvPath = do
  tmpdir <- tmpdirUnique "cr-sim"
  let simResultPath = tmpdir <</>> "sim-result.txt"
  child <- liftEffect $ spawn "steam-run"
    ( "simulator/CardanoRacersSimulator_Linux_v1.0.1.x86_64"
        : args simResultPath
    )
    defaultSpawnOptions
  liftEffect $ onDataString (stdout child) UTF8 \str -> log $ "[simulator] " <> str
  resultAvailable <- isRight <$> try (awaitResult simResultPath)
  logTrace' "Killing simulator child process..."
  void $ try $ liftEffect $ kill SIGTERM child
  if resultAvailable then do
    resultRaw <- liftAff $ readTextFile UTF8 simResultPath
    pure case String.stripPrefix (Pattern "SIM_RESULT") (String.trim resultRaw) of
      Just resultTimeStr ->
        note CouldNotConvertResultToNumber $ Number.fromString
          resultTimeStr
      Nothing ->
        Left ResultStringHasUnexpectedPrefix
  else
    pure $ Left NoResultFileAvailableAfterTimeout
  where
  awaitResult :: FilePath -> m Unit
  awaitResult fp = do
    let
      delaySec = 5
      timeoutSec = 60
    logTrace' $ "Simulator child process will be killed in " <> show timeoutSec <> " seconds"
    recovering
      ( limitRetriesByCumulativeDelay (Seconds $ Int.toNumber timeoutSec) $ constantDelay
          (Seconds $ Int.toNumber delaySec)
      )
      ([ \_ _ -> pure true ])
      ( \_ -> do
          success <- liftEffect $ exists fp
          unless success do
            logTrace' $ fp <> " does not exist yet, retrying in " <> show delaySec
              <> " seconds..."
            throwError $ error "retry"
          pure unit
      )

  option :: String -> String -> Array String
  option name val = [ "--" <> name, val ]

  args :: FilePath -> Array String
  args simResultPath = Array.concat
    [ [ "-batchmode" ]
    , [ "-logFile", "-" ]
    , option "csv" inputCsvPath
    , option "result-file" simResultPath
    ]
