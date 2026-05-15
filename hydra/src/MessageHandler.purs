module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import CardanoRacers.Hydra.Contracts.AnnounceDistr (announceRewardDistribution)
import CardanoRacers.Hydra.Contracts.Commit (commitCollateralToHydra)
import CardanoRacers.Hydra.Lib.Json (printJsonUsingCodec)
import CardanoRacers.Hydra.Lib.Retry (RetryConfig, retryOnAnyError, retryOnNothing)
import CardanoRacers.Hydra.Monad (AppM, getAppLauncher, setHydraSnapshot)
import CardanoRacers.Hydra.RewardDistribution (distributeRewards)
import CardanoRacers.Hydra.State.RaceStatus
  ( setRaceStatusAccepting
  , setRaceStatusDistributing
  , setRaceStatusFinalizing
  )
import Contract.Log (logInfo', logWarn')
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(Left, Right))
import Data.Int (round) as Int
import Data.Maybe (fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), fromDuration)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Timer (setTimeout)
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Idle)
  , HydraNodeApi_InMessage(Greetings, Committed, HeadIsOpen, SnapshotConfirmed, ReadyToFanout)
  , HydraSnapshot
  , hydraSnapshotCodec
  )

messageHandler
  :: HydraNodeApiWebSocket AppM
  -> Either String HydraNodeApi_InMessage
  -> AppM Unit
messageHandler ws msg = do
  { config: { isHeadLeader } } <- ask
  case msg of
    Left _rawMessage -> pure unit
    Right message ->
      case message of
        Greetings { headStatus, snapshotUtxo } -> do
          setHydraSnapshot $ wrap
            { snapshotNumber: zero -- FIXME: Should `Greetings` message include snapshot number?
            , utxo: fromMaybe mempty snapshotUtxo
            , confirmed: mempty
            }
          when (isHeadLeader && headStatus == HeadStatus_Idle) $
            liftEffect ws.initHead
        Committed _ ->
          -- TODO: prevent double-committing, introduce "committed" flag / barrier
          unless isHeadLeader do
            try commitCollateralToHydra >>=
              case _ of
                Left err ->
                  logWarn' $ "Could not commit collateral. Already committed? Error: "
                    <> show err
                Right txHash ->
                  logInfo' $ "Successfully commited collateral: "
                    <> show txHash
        HeadIsOpen { utxo } -> do
          setAndLogHydraSnapshot $ wrap
            { snapshotNumber: zero
            , utxo
            , confirmed: mempty
            }
          -- TODO: handle fatal errors gracefully: close Head, etc.
          { raceData } <-
            liftMaybe (error "Could not advance race status to AcceptingPlayerInputs") =<<
              setRaceStatusAccepting
          launchApp <- getAppLauncher
          liftEffect $ void $ setTimeout playerInputSubmitWindow $ launchApp do
            logInfo' "Finalizing race results..."
            do
              success <- setRaceStatusFinalizing
              unless success do
                throwError $ error "Could not advance race status to FinalizingResults"
            { finalResults } <-
              liftMaybe (error "Could not advance race status to DistributingRewards") =<<
                retryOnNothing defaultRetryConfig
                  setRaceStatusDistributing
            when isHeadLeader do
              let rewardDistr = distributeRewards finalResults raceData.raceParams
              logInfo' $ "Reward distribution: " <> show rewardDistr
              retryOnAnyError defaultRetryConfig $
                announceRewardDistribution ws rewardDistr
        SnapshotConfirmed { snapshot } -> do
          setAndLogHydraSnapshot snapshot
          when (isHeadLeader && (unwrap snapshot).snapshotNumber == one) do
            liftEffect ws.closeHead
        ReadyToFanout _ ->
          when isHeadLeader $ liftEffect ws.fanout
        _ -> pure unit

setAndLogHydraSnapshot :: HydraSnapshot -> AppM Unit
setAndLogHydraSnapshot snapshot = do
  setHydraSnapshot snapshot
  logInfo' $ "New confirmed snapshot: " <> printJsonUsingCodec hydraSnapshotCodec
    snapshot

-- TODO: time params should be configurable

playerInputSubmitWindow :: Int
playerInputSubmitWindow = Int.round $ unwrap $ fromDuration $ Minutes 5.0

defaultRetryConfig :: RetryConfig Minutes Seconds
defaultRetryConfig =
  { timeout: Minutes 5.0
  , delay: Seconds 20.0
  }
