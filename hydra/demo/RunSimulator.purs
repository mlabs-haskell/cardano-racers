module CardanoRacers.Hydra.Demo.RunSimulator
  ( main
  ) where

import Prelude

import CardanoRacers.Hydra.RaceSimulator (runSimulator)
import Control.Monad.Logger.Trans (runLoggerT)
import Data.Either (Either(Left, Right))
import Data.Log.Formatter.Pretty (prettyFormatter)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Exception (throw)
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff (readTextFile)
import Node.Process (argv)

main :: Effect Unit
main = do
  args <- argv
  case args of
    [ _, _, inputCsvPath ] ->
      launchAff_ do
        userInput <- readTextFile UTF8 inputCsvPath
        res <- flip runLoggerT (liftEffect <<< log <=< prettyFormatter) $
          runSimulator userInput
        liftEffect case res of
          Left err ->
            log $ "Simulation failed with error: " <> show err
          Right playerTime ->
            log $ "Simulation result: " <> show playerTime
    _ ->
      throw "Invalid command-line arguments"
