module CardanoRacers.Hydra.Lib.Retry
  ( RetryConfig
  , retryOnAnyError
  , retryOnFalse
  , retryOnNothing
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadError)
import Data.Maybe (Maybe, isNothing)
import Data.Time.Duration (class Duration)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (constantDelay, limitRetriesByCumulativeDelay, recovering, retrying)
import Effect.Exception (Error)

type RetryConfig (d0 :: Type) (d1 :: Type) =
  { timeout :: d0
  , delay :: d1
  }

retryOnFalse
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type)
   . MonadAff m
  => Duration d0
  => Duration d1
  => RetryConfig d0 d1
  -> m Boolean
  -> m Boolean
retryOnFalse { timeout, delay } action =
  retrying
    (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
    (\_ success -> pure $ not success)
    (\_ -> action)

retryOnNothing
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type) (a :: Type)
   . MonadAff m
  => Duration d0
  => Duration d1
  => RetryConfig d0 d1
  -> m (Maybe a)
  -> m (Maybe a)
retryOnNothing { timeout, delay } action =
  retrying
    (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
    (\_ res -> pure $ isNothing res)
    (\_ -> action)

retryOnAnyError
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type) (a :: Type)
   . MonadAff m
  => MonadError Error m
  => Duration d0
  => Duration d1
  => RetryConfig d0 d1
  -> m a
  -> m a
retryOnAnyError { timeout, delay } action =
  recovering
    (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
    [ \_ _ -> pure true ]
    (\_ -> action)
