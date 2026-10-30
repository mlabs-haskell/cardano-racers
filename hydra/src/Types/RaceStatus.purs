module CardanoRacers.Hydra.Types.RaceStatus
  ( PlayerResult
  , RaceResultSlots
  , RaceResults
  , RaceStatus
      ( Initializing
      , AcceptingPlayerInputs
      , FinalizingResults
      , DistributingRewards
      )
  , isInitializing
  , playerResultCodec
  , raceResultsCodec
  , raceResultsToMap
  ) where

import Prelude

import Aeson (Finite)
import Cardano.Plutus.Types.Address (Address) as Plutus
import CardanoRacers.Hydra.Codec (plutusAddressCodec)
import Data.Codec.Argonaut (JsonCodec, array, number, object) as CA
import Data.Codec.Argonaut.Compat (maybe) as CACompat
import Data.Codec.Argonaut.Record (record) as CAR
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.AVar (AVar)

data RaceStatus
  = Initializing
  | AcceptingPlayerInputs RaceResultSlots
  | FinalizingResults (RaceResults Maybe)
  | DistributingRewards
      { localResults :: RaceResults Maybe
      , finalResults :: RaceResults Identity
      }

derive instance Generic RaceStatus _

isInitializing :: RaceStatus -> Boolean
isInitializing =
  case _ of
    Initializing -> true
    _ -> false

type RaceResultSlots = Map Plutus.Address (AVar (Maybe (Finite Number)))

type RaceResults (f :: Type -> Type) = Array (PlayerResult f)

raceResultsCodec :: CA.JsonCodec (RaceResults Maybe)
raceResultsCodec = CA.array playerResultCodec

raceResultsToMap
  :: forall (f :: Type -> Type)
   . RaceResults f
  -> Map Plutus.Address (f (Finite Number))
raceResultsToMap =
  Map.fromFoldable
    <<< map (\{ addr, result } -> Tuple addr result)

type PlayerResult (f :: Type -> Type) =
  { addr :: Plutus.Address
  , result :: f (Finite Number)
  }

playerResultCodec :: CA.JsonCodec (PlayerResult Maybe)
playerResultCodec =
  CA.object "PlayerResult" $ CAR.record
    { addr: plutusAddressCodec
    , result: CACompat.maybe CA.number
    }
