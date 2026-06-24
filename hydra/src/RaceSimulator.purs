module CardanoRacers.Hydra.RaceSimulator
  ( RaceSimulationError
      ( NoResultFileAvailableAfterTimeout
      , ResultStringHasUnexpectedPrefix
      , CouldNotConvertResultToNumber
      )
  , raceSimulationErrorCodec
  , runSimulator
  , runSimulatorMock
  ) where

import Prelude

import Aeson (Finite, finiteNumber)
import CardanoRacers.Hydra.Lib.Retry (retryOnFalse)
import Contract.Log (logTrace')
import Control.Monad.Error.Class (class MonadError, try)
import Control.Monad.Logger.Class (class MonadLogger)
import Ctl.Internal.Helpers ((<</>>))
import Ctl.Internal.Testnet.Utils (tmpdirUnique)
import Data.Array ((:))
import Data.Array (concat) as Array
import Data.Codec.Argonaut (JsonCodec) as CA
import Data.Codec.Argonaut.Generic (nullarySum) as CAG
import Data.Either (Either(Left), note)
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
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.ChildProcess (defaultSpawnOptions, kill, spawn, stdout)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.FS.Sync (exists)
import Node.Path (FilePath)
import Node.Stream (onDataString)
import Test.QuickCheck.Gen (choose, randomSampleOne)

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

runSimulatorMock
  :: forall (m :: Type -> Type) (a :: Type)
   . MonadEffect m
  => a
  -> m (Either RaceSimulationError (Finite Number))
runSimulatorMock _ = do
  time <- liftEffect $ randomSampleOne $ choose 10.0 60.0
  pure $ note CouldNotConvertResultToNumber $ finiteNumber time

runSimulator
  :: forall (m :: Type -> Type)
   . MonadAff m
  => MonadError Error m
  => MonadLogger m
  => String
  -> m (Either RaceSimulationError (Finite Number))
runSimulator userInput = do
  tmpdir <- tmpdirUnique "cr-sim"
  let
    inputCsvPath = tmpdir <</>> "user-input.csv"
    simResultPath = tmpdir <</>> "sim-result.txt"
  liftAff $ writeTextFile UTF8 inputCsvPath userInput
  child <- liftEffect $ spawn "steam-run"
    ( "simulator/CardanoRacersSimulator_Linux_v1.0.1.x86_64"
        : args { inputCsvPath, simResultPath }
    )
    defaultSpawnOptions
  liftEffect $ onDataString (stdout child) UTF8 \str -> log $ "[simulator] " <> str
  resultAvailable <- awaitResult simResultPath
  logTrace' "Killing simulator child process..."
  void $ try $ liftEffect $ kill SIGTERM child
  if resultAvailable then do
    resultRaw <- liftAff $ readTextFile UTF8 simResultPath
    pure case String.stripPrefix (Pattern "SIM_RESULT") (String.trim resultRaw) of
      Just resultTimeStr ->
        note CouldNotConvertResultToNumber $ finiteNumber =<< Number.fromString
          resultTimeStr
      Nothing ->
        Left ResultStringHasUnexpectedPrefix
  else
    pure $ Left NoResultFileAvailableAfterTimeout
  where
  awaitResult :: FilePath -> m Boolean
  awaitResult fp = do
    let
      delaySec = 5
      timeoutSec = 60
    logTrace' $ "Simulator child process will be killed in " <> show timeoutSec <> " seconds"
    retryOnFalse
      { timeout: Seconds $ Int.toNumber timeoutSec
      , delay: Seconds $ Int.toNumber delaySec
      }
      ( do
          success <- liftEffect $ exists fp
          unless success do
            logTrace' $ fp <> " does not exist yet, retrying in " <> show delaySec
              <> " seconds..."
          pure success
      )

  option :: String -> String -> Array String
  option name val = [ "--" <> name, val ]

  args :: { inputCsvPath :: FilePath, simResultPath :: FilePath } -> Array String
  args { inputCsvPath, simResultPath } = Array.concat
    [ [ "-batchmode" ]
    , [ "-logFile", "-" ]
    , option "csv" inputCsvPath
    , option "result-file" simResultPath
    ]
