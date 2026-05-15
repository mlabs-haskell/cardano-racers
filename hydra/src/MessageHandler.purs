module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import Cardano.Plutus.Types.Map (empty) as Plutus.Map
import CardanoRacers.Hydra.Contracts.AnnounceDistr (announceRewardDistribution)
import CardanoRacers.Hydra.Contracts.Commit (commitCollateralToHydra)
import CardanoRacers.Hydra.Lib.Json (printJsonUsingCodec)
import CardanoRacers.Hydra.Lib.Retry (retryBool)
import CardanoRacers.Hydra.Monad (AppM, getAppLauncher, setHydraSnapshot)
import CardanoRacers.Hydra.State.RaceStatus
  ( setRaceStatusAccepting
  , setRaceStatusDistributing
  , setRaceStatusFinalizing
  )
import Contract.Log (logInfo', logWarn')
import Control.Monad.Error.Class (throwError, try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap, unwrap)
import Data.Time.Duration (Minutes(Minutes), Seconds(Seconds))
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
          do
            success <- setRaceStatusAccepting
            -- TODO: handle fatal errors gracefully: close Head, etc.
            unless success do
              throwError $ error "Could not advance race status to AcceptingPlayerInputs"
          launchApp <- getAppLauncher
          -- TODO: time params should be configurable
          liftEffect $ void $ setTimeout 300000 {- 5 min -}  $ launchApp do
            logInfo' "Finalizing race results..."
            do
              success <- setRaceStatusFinalizing
              unless success do
                throwError $ error "Could not advance race status to FinalizingResults"
            do
              success <-
                retryBool { timeout: Minutes 5.0, delay: Seconds 10.0 }
                  setRaceStatusDistributing
              unless success do
                throwError $ error "Could not advance race status to DistributingRewards"
            when isHeadLeader do
              -- TODO: build reward distribution
              announceRewardDistribution ws Plutus.Map.empty
        SnapshotConfirmed { snapshot } -> do
          setAndLogHydraSnapshot snapshot
          when (isHeadLeader && (unwrap snapshot).snapshotNumber == 1) do
            liftEffect ws.closeHead
        ReadyToFanout _ ->
          when isHeadLeader $ liftEffect ws.fanout
        _ -> pure unit

setAndLogHydraSnapshot :: HydraSnapshot -> AppM Unit
setAndLogHydraSnapshot snapshot = do
  setHydraSnapshot snapshot
  logInfo' $ "New confirmed snapshot: " <> printJsonUsingCodec hydraSnapshotCodec
    snapshot
