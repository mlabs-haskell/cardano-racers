module Lib.CardanoRacers.ClientFFI (module X, mkClientFFI) where

import CardanoRacers.Common.Types (RacersParams)
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Client (Client, mkClient)
import Lib.CardanoRacers.Common (CredentialProvider)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

mkClientFFI
  :: Fn2 CredentialProvider RacersParams (Record (Client + Queries + ()))
mkClientFFI = mkFn2 mkClient
