module Lib.CardanoRacers.QueriesFFI (module X, mkQueriesFFI) where

import CardanoRacers.Common.Types (RacersParams)
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Common (CredentialProvider)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries, mkQueries)

mkQueriesFFI
  :: Fn2 CredentialProvider RacersParams (Record (Queries ()))
mkQueriesFFI = mkFn2 mkQueries

