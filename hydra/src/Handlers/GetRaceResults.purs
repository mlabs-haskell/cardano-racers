module CardanoRacers.Hydra.Handlers.GetRaceResults
  ( RaceParticipantResult
  , GetRaceResultsError(RaceResultsNotAvailable, EmptyResultSlot)
  , RaceResults
  , getRaceResults
  , getRaceResultsHandler
  , raceParticipantResultCodec
  , raceResultsCodec
  , raceResultsToMap
  ) where

import Prelude

import Aeson (Finite, stringifyAeson)
import Cardano.Plutus.Types.Address (Address) as Plutus
import CardanoRacers.Hydra.Codec (plutusAddressCodec)
import CardanoRacers.Hydra.Lib.AVar (readNow) as AVar
import CardanoRacers.Hydra.Monad (AppM, RaceResultSlots)
import Control.Error.Util ((!?))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (ask)
import Data.Codec.Argonaut (JsonCodec, array, encode, number, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CACompat
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Codec.Argonaut.Sum (sumFlat) as CAS
import Data.Either (Either(Left, Right))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map (fromFoldable, toUnfoldable) as Map
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (tryRead) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)
import HTTPure (Response) as HTTPure
import HTTPure (Status, ok, response)
import HTTPure.Status (conflict, internalServerError) as Status

type RaceResults (f :: Type -> Type) = Array (RaceParticipantResult f)

raceResultsCodec :: CA.JsonCodec (RaceResults Maybe)
raceResultsCodec = CA.array raceParticipantResultCodec

raceResultsToMap
  :: forall (f :: Type -> Type)
   . RaceResults f
  -> Map Plutus.Address (f (Finite Number))
raceResultsToMap =
  Map.fromFoldable
    <<< map (\{ participant, result } -> Tuple participant result)

type RaceParticipantResult (f :: Type -> Type) =
  { participant :: Plutus.Address
  , result :: f (Finite Number)
  }

raceParticipantResultCodec :: CA.JsonCodec (RaceParticipantResult Maybe)
raceParticipantResultCodec =
  CA.object "RaceParticipantResult" $ CAR.record
    { participant: plutusAddressCodec
    , result: CACompat.maybe CA.number
    }

getRaceResultsHandler :: AppM HTTPure.Response
getRaceResultsHandler = do
  { resultSlots } <- ask
  getRaceResults resultSlots >>=
    case _ of
      Left err ->
        response (errorStatus err) $ stringifyAeson $ CA.encode getRaceResultsErrorCodec err
      Right res ->
        ok $ stringifyAeson $ CA.encode raceResultsCodec res

getRaceResults
  :: forall (m :: Type -> Type)
   . MonadAff m
  => AVar RaceResultSlots
  -> m (Either GetRaceResultsError (RaceResults Maybe))
getRaceResults resultSlots =
  runExceptT do
    slots <- Map.toUnfoldable <$> liftAff (AVar.tryRead resultSlots) !?
      RaceResultsNotAvailable
    traverse
      ( \(Tuple participant slot) ->
          AVar.readNow EmptyResultSlot slot <#> \result ->
            { participant
            , result
            }
      )
      slots

-- Errors

data GetRaceResultsError
  = RaceResultsNotAvailable
  | EmptyResultSlot

derive instance Generic GetRaceResultsError _
derive instance Eq GetRaceResultsError

instance Show GetRaceResultsError where
  show = genericShow

getRaceResultsErrorCodec :: CA.JsonCodec GetRaceResultsError
getRaceResultsErrorCodec =
  CAS.sumFlat "GetRaceResultsError"
    { "RaceResultsNotAvailable": unit
    , "EmptyResultSlot": unit
    }

errorStatus :: GetRaceResultsError -> Status
errorStatus =
  case _ of
    RaceResultsNotAvailable ->
      Status.conflict
    EmptyResultSlot ->
      Status.internalServerError
