module CardanoRacers.Hydra.State.RaceStatus
  ( setRaceStatusAccepting
  , setRaceStatusFinalizing
  , setRaceStatusDistributing
  ) where

import Prelude

import CardanoRacers.Hydra.Monad (AppM)
import CardanoRacers.Hydra.ResultsConsensus (confirmResultsByConsensus)
import CardanoRacers.Hydra.Types.RaceStatus
  ( RaceStatus(Initializing, AcceptingPlayerInputs, FinalizingResults, DistributingRewards)
  , isInitializing
  )
import Contract.Log (logError', logInfo')
import Control.Monad.Reader (ask)
import Data.Either (Either(Left, Right))
import Data.Map (fromFoldable, toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.AVar (new, read) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (read, write) as Ref

setRaceStatusAccepting :: AppM Boolean
setRaceStatusAccepting = do
  { raceDataRef, raceStatusRef } <- ask
  raceData <- liftEffect $ Ref.read raceDataRef
  raceStatus <- liftEffect $ Ref.read raceStatusRef
  case raceData, raceStatus of
    Just rd, Initializing -> do
      resultSlots <-
        Map.fromFoldable <$>
          traverse
            (\addr -> Tuple addr <$> liftAff (AVar.new Nothing))
            (unwrap rd.raceParams).participants
      liftEffect $ Ref.write (AcceptingPlayerInputs resultSlots) raceStatusRef
      pure true
    x, y -> do
      when (isNothing x) $
        logError' "setRaceStatusAccepting: race data is not set"
      unless (isInitializing y) $
        logError' "setRaceStatusAccepting: unexpected race status"
      pure false

setRaceStatusFinalizing :: AppM Boolean
setRaceStatusFinalizing = do
  { raceStatusRef } <- ask
  raceStatus <- liftEffect $ Ref.read raceStatusRef
  case raceStatus of
    AcceptingPlayerInputs resultSlots -> do
      results <- liftAff $ traverse
        ( \(Tuple addr slot) ->
            AVar.read slot <#> \result ->
              { addr
              , result
              }
        )
        (Map.toUnfoldableUnordered resultSlots)
      liftEffect $ Ref.write (FinalizingResults results) raceStatusRef
      pure true
    _ -> do
      logError' "setRaceStatusFinalizing: unexpected race status"
      pure false

setRaceStatusDistributing :: AppM Boolean
setRaceStatusDistributing = do
  { raceStatusRef, config: { hydraNodeStartupParams: { peers } } } <- ask
  raceStatus <- liftEffect $ Ref.read raceStatusRef
  case raceStatus of
    FinalizingResults localResults ->
      confirmResultsByConsensus localResults (_.httpServer <$> peers) >>=
        case _ of
          Left err -> do
            logError' $
              "setRaceStatusDistributing: could not finalize race results. error: "
                <> show err
            pure false
          Right finalResults -> do
            logInfo' $ "setRaceStatusDistributing: final race results reached by consensus: "
              <> show finalResults
            liftEffect $ flip Ref.write raceStatusRef $ DistributingRewards
              { localResults
              , finalResults
              }
            pure true
    _ -> do
      logError' "setRaceStatusDistributing: unexpected race status"
      pure false
