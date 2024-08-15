module Lib.CardanoRacers.AdminFFI
  ( module X
  , mkAdmin
  , initRacers
  ) where

import Contract.Prelude

import Aeson (encodeAeson)
import CardanoRacers.Common.Types (RacersParams)
import Contract.Config (ContractParams, WalletSpec)
import Control.Promise (Promise, fromAff)
import Data.Function.Uncurried (Fn3, mkFn3)
import Effect.Aff.Compat (EffectFn3, mkEffectFn3)
import Lib.CardanoRacers.Admin (Admin, InitialStateFFI)
import Lib.CardanoRacers.Admin (initRacers, mkAdmin) as Admin
import Lib.CardanoRacers.Bot (Bot)
import Lib.CardanoRacers.Common (contractParams, mkRacersParams) as X
import Lib.CardanoRacers.Queries (Queries)
import Type.Row (type (+))

initRacers
  :: EffectFn3 ContractParams WalletSpec InitialStateFFI (Promise String)
initRacers = mkEffectFn3 \cp w is -> fromAff $ Admin.initRacers cp w is <#>
  (encodeAeson >>> show)

mkAdmin
  :: Fn3 ContractParams WalletSpec RacersParams
       (Record (Admin + Bot + Queries + ()))
mkAdmin = mkFn3 Admin.mkAdmin
