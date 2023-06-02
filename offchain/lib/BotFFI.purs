module Lib.CardanoRacers.BotFFI (module X, mkBotFFI) where

import CardanoRacers.Common.Types (RacersParams)
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Bot (Bot, mkBot)
import Lib.CardanoRacers.Common (CredentialProvider)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

mkBotFFI
  :: Fn2 CredentialProvider RacersParams (Record (Bot + Queries + ()))
mkBotFFI = mkFn2 mkBot
