module CardanoRacers.Hydra.MessageHandler
  ( messageHandler
  ) where

import Prelude

import CardanoRacers.Hydra.Monad (AppM)
import Data.Either (Either(Left, Right))
import HydraSdk.NodeApi (HydraNodeApiWebSocket)
import HydraSdk.Types (HydraNodeApi_InMessage(Greetings))

messageHandler
  :: HydraNodeApiWebSocket AppM
  -> Either String HydraNodeApi_InMessage
  -> AppM Unit
messageHandler _ws =
  case _ of
    Left _rawMessage -> pure unit
    Right message ->
      case message of
        Greetings _ -> pure unit
        _ -> pure unit
