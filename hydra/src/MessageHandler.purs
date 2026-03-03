module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import Cardano.Plutus.Types.Map (empty) as Plutus.Map
import CardanoRacers.Hydra.Contracts.AnnounceDistr (announceRewardDistribution)
import CardanoRacers.Hydra.Contracts.Commit (commitCollateralToHydra)
import CardanoRacers.Hydra.Lib.Json (printJsonUsingCodec)
import CardanoRacers.Hydra.Monad (AppM, setHydraSnapshot)
import Contract.Log (logInfo', logWarn')
import Control.Monad.Error.Class (try)
import Control.Monad.Reader.Class (ask)
import Data.Either (Either(Left, Right))
import Data.Maybe (fromMaybe)
import Data.Newtype (wrap, unwrap)
import Effect.Class (liftEffect)
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
          -- FIXME: remove later
          -- Submitting an empty reward distribution once the Head is open,
          -- for testing purposes
          when isHeadLeader do
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
