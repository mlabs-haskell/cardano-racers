module CardanoRacers.Hydra.State.RaceStatus
  ( setRaceStatusAccepting
  , setRaceStatusFinalizing
  , setRaceStatusDistributing
  ) where

import Prelude

import CardanoRacers.Hydra.Monad (AppM, RaceData, RaceEntry, printRaceId)
import CardanoRacers.Hydra.ResultsConsensus (confirmResultsByConsensus)
import CardanoRacers.Hydra.Types.RaceStatus
  ( RaceStatus(Initializing, AcceptingPlayerInputs, FinalizingResults, DistributingRewards)
  , RaceResults
  , isInitializing
  )
import Contract.Log (logError', logInfo')
import Control.Monad.Reader (ask)
import Data.Either (Either(Left, Right))
import Data.Identity (Identity)
import Data.Map (fromFoldable, toUnfoldableUnordered) as Map
import Data.Maybe (Maybe(Just, Nothing), isNothing)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(Tuple))
import Effect.Aff.AVar (new, read) as AVar
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Ref (read, write) as Ref

setRaceStatusAccepting :: RaceEntry -> AppM Boolean
setRaceStatusAccepting { raceData, raceStatusRef } = do
  raceStatus <- liftEffect $ Ref.read raceStatusRef
  case raceStatus of
    Initializing -> do
      resultSlots <-
        Map.fromFoldable <$>
          traverse
            (\addr -> Tuple addr <$> liftAff (AVar.new Nothing))
            (unwrap raceData.raceParams).participants
      liftEffect $ Ref.write (AcceptingPlayerInputs resultSlots) raceStatusRef
      pure true
    _ -> do
      logError' $ "setRaceStatusAccepting: unexpected race status for race: " <>
        printRaceId raceData
      pure false

setRaceStatusFinalizing :: RaceEntry -> AppM Boolean
setRaceStatusFinalizing { raceData, raceStatusRef } = do
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
      logError' $ "setRaceStatusFinalizing: unexpected race status for race: " <>
        printRaceId raceData
      pure false

setRaceStatusDistributing
  :: RaceEntry
  -> AppM
       ( Maybe
           { finalResults :: RaceResults Identity
           }
       )
setRaceStatusDistributing { raceData, raceStatusRef } = do
  { config: { hydraNodeStartupParams: { peers } } } <- ask
  raceStatus <- liftEffect $ Ref.read raceStatusRef
  case raceStatus of
    FinalizingResults localResults -> do
      let raceCs = (unwrap raceData.raceParams).stateCurrencySymbol
      confirmResultsByConsensus raceCs localResults (_.httpServer <$> peers) >>=
        case _ of
          Left err -> do
            logError' $
              "setRaceStatusDistributing: could not finalize race results. error: "
                <> show err
            pure Nothing
          Right finalResults -> do
            logInfo' $ "setRaceStatusDistributing: final race results reached by consensus: "
              <> show finalResults
            liftEffect $ flip Ref.write raceStatusRef $ DistributingRewards
              { localResults
              , finalResults
              }
            pure $ Just { finalResults }
    _ -> do
      logError' $ "setRaceStatusDistributing: unexpected race status for race: " <>
        printRaceId raceData
      pure Nothing
