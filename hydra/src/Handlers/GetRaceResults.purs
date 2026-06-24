module CardanoRacers.Hydra.Handlers.GetRaceResults
  ( GetRaceResultsError
      ( CouldNotDecodeRaceCs
      , RequestedRaceNotHosted
      , RaceResultsNotAvailable
      )
  , getRaceResultsErrorCodec
  , getRaceResultsHandler
  ) where

import Prelude

import Aeson (stringifyAeson)
import Cardano.AsCbor (decodeCbor)
import CardanoRacers.Hydra.Monad (AppM, findRaceEntryByRaceCs)
import CardanoRacers.Hydra.Types.RaceStatus
  ( RaceResults
  , RaceStatus(FinalizingResults, DistributingRewards)
  , raceResultsCodec
  )
import Contract.CborBytes (hexToCborBytes)
import Control.Error.Util ((!?), (??))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect.Class (liftEffect)
import Effect.Ref (read) as Ref
import HTTPure (Response) as HTTPure
import HTTPure (Status, ok, response)
import HTTPure.Status (badRequest, conflict) as Status

getRaceResultsHandler :: String -> AppM HTTPure.Response
getRaceResultsHandler raceCsStr =
  getRaceResults raceCsStr >>=
    case _ of
      Left err ->
        response (errorStatus err) $ stringifyAeson $ CA.encode getRaceResultsErrorCodec err
      Right res ->
        ok $ stringifyAeson $ CA.encode raceResultsCodec res

getRaceResults :: String -> AppM (Either GetRaceResultsError (RaceResults Maybe))
getRaceResults raceCsStr =
  runExceptT do
    raceCs <- (decodeCbor =<< hexToCborBytes raceCsStr) ?? CouldNotDecodeRaceCs
    { raceStatusRef } <- findRaceEntryByRaceCs raceCs !? RequestedRaceNotHosted
    raceStatus <- liftEffect $ Ref.read raceStatusRef
    case raceStatus of
      FinalizingResults raceResults -> pure raceResults
      DistributingRewards { localResults } -> pure localResults
      _ -> throwError RaceResultsNotAvailable

-- Errors

data GetRaceResultsError
  = CouldNotDecodeRaceCs
  | RequestedRaceNotHosted
  | RaceResultsNotAvailable

derive instance Generic GetRaceResultsError _
derive instance Eq GetRaceResultsError

instance Show GetRaceResultsError where
  show = genericShow

getRaceResultsErrorCodec :: CA.JsonCodec GetRaceResultsError
getRaceResultsErrorCodec =
  CAS.sumFlat "GetRaceResultsError"
    { "CouldNotDecodeRaceCs": unit
    , "RequestedRaceNotHosted": unit
    , "RaceResultsNotAvailable": unit
    }

errorStatus :: GetRaceResultsError -> Status
errorStatus =
  case _ of
    CouldNotDecodeRaceCs ->
      Status.badRequest
    RequestedRaceNotHosted ->
      Status.conflict
    RaceResultsNotAvailable ->
      Status.conflict
