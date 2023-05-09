module Racers (Racers, RacersEnv, runRacers, withContract) where

import Contract.Prelude

import CardanoRacers.Common.Types (RacersParams)
import Contract.Monad (Contract)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Trans (asks)
import Control.Monad.Trans.Class (lift)

type RacersEnv = { params :: RacersParams }

type Racers = ReaderT RacersEnv Contract

runRacers :: forall a. RacersParams -> Racers a -> Contract a
runRacers rp = flip runReaderT { params: rp }

withContract :: forall a b. (Contract a -> Contract b) -> Racers a -> Racers b
withContract f c = do
  rp <- asks _.params
  lift $ f (runRacers rp c)
