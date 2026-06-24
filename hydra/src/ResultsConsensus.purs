module CardanoRacers.Hydra.ResultsConsensus
  ( ConfirmResultsError
      ( CouldNotGetOwnResults
      , CouldNotGetPeerResults_RequestFailed
      , CouldNotGetPeerResults_EndpointReturnedError
      )
  , confirmResultsByConsensus
  ) where

import Prelude

import Cardano.Provider (ServerConfig)
import Cardano.Provider.ServerConfig (mkHttpUrl)
import Cardano.Types (ScriptHash)
import CardanoRacers.Hydra.Handlers.GetRaceResults (GetRaceResultsError)
import CardanoRacers.Hydra.Services.HydraPeer (getRaceResultsRequest)
import CardanoRacers.Hydra.Types.RaceStatus (RaceResults, raceResultsToMap)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Data.Array ((:))
import Data.Array (catMaybes, sortWith) as Array
import Data.Either (Either(Left, Right))
import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(Identity))
import Data.Map (empty, toUnfoldableUnordered, unionWith) as Map
import Data.Maybe (Maybe(Nothing))
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff, liftAff)
import HydraSdk.Types (HttpError)

data ConfirmResultsError
  = CouldNotGetOwnResults GetRaceResultsError
  | CouldNotGetPeerResults_RequestFailed HttpError
  | CouldNotGetPeerResults_EndpointReturnedError GetRaceResultsError

derive instance Generic ConfirmResultsError _

instance Show ConfirmResultsError where
  show = genericShow

confirmResultsByConsensus
  :: forall (m :: Type -> Type)
   . MonadAff m
  => ScriptHash
  -> RaceResults Maybe
  -> Array ServerConfig
  -> m (Either ConfirmResultsError (RaceResults Identity))
confirmResultsByConsensus raceCs localResults peers =
  runExceptT do
    let localResultMap = raceResultsToMap localResults
    peerResultMaps <- traverse
      ( \httpServer ->
          liftAff (getRaceResultsRequest (mkHttpUrl httpServer) raceCs) >>=
            case _ of
              Left httpErr ->
                throwError $ CouldNotGetPeerResults_RequestFailed httpErr
              Right (Left domainErr) ->
                throwError $ CouldNotGetPeerResults_EndpointReturnedError domainErr
              Right (Right results) ->
                pure $ raceResultsToMap results
      )
      peers
    let
      resultMaps = localResultMap : peerResultMaps
      finalResults =
        foldl
          (Map.unionWith (\x y -> if x == y then x else Nothing))
          Map.empty
          resultMaps
    pure $ Array.sortWith _.result $ Array.catMaybes
      ( Map.toUnfoldableUnordered finalResults <#> \(addr /\ mResult) ->
          mResult <#> \result ->
            { addr
            , result: Identity result
            }
      )
