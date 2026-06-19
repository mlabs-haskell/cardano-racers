module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import CardanoRacers.Hydra.Contracts.AnnounceDistr (announceRewardDistribution)
import CardanoRacers.Hydra.Contracts.Commit (commitCollateralToHydra)
import CardanoRacers.Hydra.Lib.Json (printJsonUsingCodec)
import CardanoRacers.Hydra.Lib.Retry (RetryConfig, retryOnAnyError, retryOnNothing)
import CardanoRacers.Hydra.Monad
  ( AppM
  , RaceEntry
  , findRaceEntryByDepositTxId
  , getAppLauncher
  , printRaceId
  , readHeadStatus
  , removeRaceEntry
  , setHydraSnapshot
  )
import CardanoRacers.Hydra.RewardDistribution (distributeRewards)
import CardanoRacers.Hydra.State.RaceStatus
  ( setRaceStatusAccepting
  , setRaceStatusDistributing
  , setRaceStatusFinalizing
  )
import CardanoRaces.Hydra.Lib.Print (printHex)
import Contract.Log (logError', logInfo', logWarn')
import Control.Monad.Error.Class (catchError, liftMaybe, throwError, try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(Left, Right))
import Data.Int (round) as Int
import Data.Maybe (Maybe(Just, Nothing), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds), fromDuration)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Effect.Exception (message) as Error
import Effect.Timer (setTimeout)
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Unknown, HeadStatus_Idle, HeadStatus_Open, HeadStatus_Final)
  , HydraNodeApi_InMessage
      ( Greetings
      , NodeSynced
      , HeadIsOpen
      , DepositExpired
      , CommitRecorded
      , CommitFinalized
      , DecommitFinalized
      , SnapshotConfirmed
      , ReadyToFanout
      )
  , HydraSnapshot
  , SyncStatus(InSync)
  , hydraSnapshotCodec
  , printHeadStatus
  )

messageHandler
  :: HydraNodeApiWebSocket AppM
  -> Either String HydraNodeApi_InMessage
  -> AppM Unit
messageHandler ws msg = do
  { config: { isHeadLeader } } <- ask
  closeHeadOnException ws case msg of
    Left _rawMessage -> pure unit
    Right message ->
      case message of
        Greetings { headStatus, snapshotUtxo, chainSyncedStatus } -> do
          setHydraSnapshot $ wrap
            { number: zero -- FIXME(low-prio): Should `Greetings` message include snapshot number?
            , utxo: fromMaybe mempty snapshotUtxo
            , confirmed: mempty
            , utxoToCommit: Nothing
            , utxoToDecommit: Nothing
            }
          when (isHeadLeader && headStatus == HeadStatus_Idle && chainSyncedStatus == InSync) $
            liftEffect ws.initHead
        NodeSynced -> do
          headStatus <- readHeadStatus
          when (isHeadLeader && headStatus == HeadStatus_Idle) $
            liftEffect ws.initHead
        HeadIsOpen _ ->
          -- Head leader commits ADA-only collateral UTxO to be reused when
          -- constructing reward distribution txs
          when isHeadLeader do
            try commitCollateralToHydra >>=
              case _ of
                Left err ->
                  logWarn' $ "Could not commit collateral. Already committed? Error: "
                    <> show err
                Right txHash ->
                  logInfo' $ "Successfully commited collateral: "
                    <> show txHash
        DepositExpired { depositTxId, deadline: _deadline } -> do
          logWarn' $ "Deposit expired. Deposit TxId: " <> printHex depositTxId
          removeRaceEntry depositTxId
        CommitRecorded { pendingDeposit: depositTxId } -> do
          logInfo' $ "Commit recorded. Deposit TxId: " <> printHex depositTxId
        -- TODO(med-prio): recover deposit
        CommitFinalized { depositTxId } -> do
          logInfo' $ "Commit finalized. Deposit TxId: " <> printHex depositTxId
          findRaceEntryByDepositTxId depositTxId >>=
            case _ of
              Just race -> do
                logInfo' $ "Deposit finalized for race: " <> printRaceId race.raceData
                -- Errors for individual races are suppressed and logged to not
                -- affect other races
                processRace ws race `catchError` \err ->
                  logError' $ "Could not process race " <> printRaceId race.raceData
                    <> ", error: "
                    <> Error.message err
              Nothing ->
                pure unit -- collateral deposit?
        DecommitFinalized { distributedUTxO: distributedUtxos } -> do
          logInfo' $ "Decommit finalized. Distributed UTxOs: " <> show distributedUtxos
        SnapshotConfirmed { snapshot } ->
          setAndLogHydraSnapshot snapshot
        ReadyToFanout _ ->
          when isHeadLeader do
            liftEffect ws.fanout
        _ -> pure unit

-- TODO(low-prio): revise error handling approach
closeHeadOnException :: HydraNodeApiWebSocket AppM -> AppM Unit -> AppM Unit
closeHeadOnException ws action = do
  action `catchError` \err -> do
    headStatus <- readHeadStatus
    logError' $ "Got unrecoverable exception. Head status: " <>
      printHeadStatus headStatus
    case headStatus of
      HeadStatus_Unknown ->
        throwError err
      HeadStatus_Idle ->
        throwError err
      HeadStatus_Final ->
        throwError err
      HeadStatus_Open -> do
        logError' "Closing the Head to apply the current Hydra snapshot to mainchain..."
        liftEffect ws.closeHead
      _ -> do
        logError' "Ignoring exception..."

processRace :: HydraNodeApiWebSocket AppM -> RaceEntry -> AppM Unit
processRace ws race@{ raceData } = do
  { config: { isHeadLeader, timeParams } } <- ask
  do
    success <- setRaceStatusAccepting race
    unless success do
      throwError $ error "Could not advance race status to AcceptingPlayerInputs"
  launchApp <- getAppLauncher
  -- TODO(med-prio): cancel timers as part of cleanup 
  liftEffect $ void $ setTimeout (timeParams.playerInputSubmitWindowSec * 1000) $
    launchApp do
      logInfo' "Finalizing race results..."
      do
        success <- setRaceStatusFinalizing race
        unless success do
          throwError $ error "Could not advance race status to FinalizingResults"
      { finalResults } <-
        liftMaybe (error "Could not advance race status to DistributingRewards")
          =<<
            retryOnNothing defaultRetryConfig
              (setRaceStatusDistributing race)
      when isHeadLeader do
        let rewardDistr = distributeRewards finalResults raceData.raceParams
        logInfo' $ "Reward distribution: " <> show rewardDistr
        retryOnAnyError "announceRewardDistribution" defaultRetryConfig $
          announceRewardDistribution ws raceData rewardDistr

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
