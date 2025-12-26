module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import CardanoRacers.Hydra.Contracts.Commit (commitCollateralToHydra)
import CardanoRacers.Hydra.Monad (AppM)
import Contract.Log (logInfo', logWarn')
import Control.Monad.Error.Class (try)
import Data.Either (Either(Left, Right))
import Effect.Class (liftEffect)
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Types
  ( HydraHeadStatus(HeadStatus_Idle)
  , HydraNodeApi_InMessage(Greetings, Committed)
  )

messageHandler
  :: HydraNodeApiWebSocket AppM
  -> Either String HydraNodeApi_InMessage
  -> AppM Unit
messageHandler ws =
  case _ of
    Left _rawMessage -> pure unit
    Right message ->
      case message of
        Greetings { headStatus } ->
          -- TODO: ensure only one Head member calls initHead
          when (headStatus == HeadStatus_Idle) $
            liftEffect ws.initHead
        Committed _ ->
          -- TODO: prevent double-committing, introduce "committed" flag / barrier
          try commitCollateralToHydra >>=
            case _ of
              Left err ->
                logWarn' $ "Could not commit collateral. Already committed? Error: "
                  <> show err
              Right txHash ->
                logInfo' $ "Successfully commited collateral: "
                  <> show txHash
        _ -> pure unit
