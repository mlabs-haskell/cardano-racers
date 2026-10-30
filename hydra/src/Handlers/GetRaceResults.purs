module CardanoRacers.Hydra.Handlers.GetRaceResults
  ( GetRaceResultsError(RaceResultsNotAvailable)
  , getRaceResultsErrorCodec
  , getRaceResultsHandler
  ) where

import Prelude

import Aeson (stringifyAeson)
import CardanoRacers.Hydra.Monad (AppM)
import CardanoRacers.Hydra.Types.RaceStatus
  ( RaceResults
  , RaceStatus(FinalizingResults, DistributingRewards)
  , raceResultsCodec
  )
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Codec.Argonaut (JsonCodec, encode) as CA
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref (read) as Ref
import HTTPure (Response) as HTTPure
import HTTPure (Status, ok, response)
import HTTPure.Status (conflict) as Status

getRaceResultsHandler :: AppM HTTPure.Response
getRaceResultsHandler = do
  { raceStatusRef } <- ask
  getRaceResults raceStatusRef >>=
    case _ of
      Left err ->
        response (errorStatus err) $ stringifyAeson $ CA.encode getRaceResultsErrorCodec err
      Right res ->
        ok $ stringifyAeson $ CA.encode raceResultsCodec res

getRaceResults
  :: forall (m :: Type -> Type)
   . MonadEffect m
  => Ref RaceStatus
  -> m (Either GetRaceResultsError (RaceResults Maybe))
getRaceResults raceStatusRef =
  runExceptT do
    raceStatus <- liftEffect $ Ref.read raceStatusRef
    case raceStatus of
      FinalizingResults raceResults -> pure raceResults
      DistributingRewards { localResults } -> pure localResults
      _ -> throwError RaceResultsNotAvailable

-- Errors

data GetRaceResultsError = RaceResultsNotAvailable

derive instance Generic GetRaceResultsError _
derive instance Eq GetRaceResultsError

instance Show GetRaceResultsError where
  show = genericShow

getRaceResultsErrorCodec :: CA.JsonCodec GetRaceResultsError
getRaceResultsErrorCodec =
  CAS.sumFlat "GetRaceResultsError"
    { "RaceResultsNotAvailable": unit
    }

errorStatus :: GetRaceResultsError -> Status
errorStatus =
  case _ of
    RaceResultsNotAvailable ->
      Status.conflict
