module Lib.CardanoRacers.AdminFFI (module X, mkAdminFFI) where

import CardanoRacers.Common.Types (RacersParams)
import Data.Function.Uncurried (Fn2, mkFn2)
import Lib.CardanoRacers.Admin (Admin, mkAdmin)
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Common (CredentialProvider)
import Lib.CardanoRacers.Common (mkCredentialProviderFFI, mkRacersParamsFFI) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

mkAdminFFI
  :: Fn2 CredentialProvider RacersParams (Record (Admin + Bot + Queries + ()))
mkAdminFFI = mkFn2 mkAdmin
