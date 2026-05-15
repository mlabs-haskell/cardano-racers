module CardanoRacers.Hydra.Lib.Retry
  ( retryBool
  ) where

import Prelude

import Data.Time.Duration (class Duration)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (constantDelay, limitRetriesByCumulativeDelay, retrying)

retryBool
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type) (a :: Type)
   . MonadAff m
  => Duration d0
  => Duration d1
  => { timeout :: d0
     , delay :: d1
     }
  -> m Boolean
  -> m Boolean
retryBool { timeout, delay } action =
  retrying
    (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
    (\_ success -> pure $ not success)
    (\_ -> action)
