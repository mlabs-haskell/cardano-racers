module Lib.CardanoRacers.ClientFFI (module X, mkClient) where

import CardanoRacers.Common.Types (RacersParams)
import Contract.Config (ContractParams, WalletSpec)
import Data.Function.Uncurried (Fn3, mkFn3)
import Lib.CardanoRacers.Client (Client)
import Lib.CardanoRacers.Client (mkClient) as Client
import Lib.CardanoRacers.Common (contractParams, mkRacersParams, walletSpec) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

mkClient
  :: Fn3 ContractParams WalletSpec RacersParams (Record (Client + Queries + ()))
mkClient = mkFn3 Client.mkClient
