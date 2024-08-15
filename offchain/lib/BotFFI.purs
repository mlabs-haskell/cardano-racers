module Lib.CardanoRacers.BotFFI (module X, mkBot) where

import CardanoRacers.Common.Types (RacersParams)
import Contract.Config (ContractParams)
import Data.Function.Uncurried (Fn3, mkFn3)
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Bot (mkBot) as Bot
import Lib.CardanoRacers.Common (CredentialProvider)
import Lib.CardanoRacers.Common (contractParams, mkRacersParams) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

mkBot
  :: Fn3 ContractParams CredentialProvider RacersParams
       (Record (Bot + Queries + ()))
mkBot = mkFn3 Bot.mkBot
