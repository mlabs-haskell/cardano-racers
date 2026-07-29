module CardanoRacers.Hydra.Lib.Retry
  ( RetryConfig
  , retryOnAnyError
  , retryOnError
  , retryOnFalse
  , retryOnNothing
  ) where

import Prelude

import Contract.Log (logWarn')
import Control.Monad.Error.Class (class MonadError, class MonadThrow, throwError, try)
import Control.Monad.Logger.Class (class MonadLogger)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe, isNothing)
import Data.Time.Duration (class Duration)
import Effect.Aff.Class (class MonadAff)
import Effect.Aff.Retry (constantDelay, limitRetriesByCumulativeDelay, recovering, retrying)
import Effect.Exception (Error, error)
import Effect.Exception (message) as Error

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

retryOnError
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type) (e :: Type) (a :: Type)
   . MonadAff m
  => MonadThrow Error m
  => MonadLogger m
  => Duration d0
  => Duration d1
  => Show e
  => String
  -> (e -> m Boolean)
  -> RetryConfig d0 d1
  -> m (Either e a)
  -> m a
retryOnError label predicate { timeout, delay } action =
  either (throwError <<< error <<< show) pure =<<
    retrying
      (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
      (\_ -> either predicate (const (pure false)))
      ( \_ ->
          action >>=
            case _ of
              Left err -> do
                logWarn' $
                  "retryOnError: action with label "
                    <> label
                    <> " has failed with error: "
                    <> show err
                pure $ Left err
              x -> pure x
      )

retryOnAnyError
  :: forall (m :: Type -> Type) (d0 :: Type) (d1 :: Type) (a :: Type)
   . MonadAff m
  => MonadError Error m
  => MonadLogger m
  => Duration d0
  => Duration d1
  => String
  -> RetryConfig d0 d1
  -> m a
  -> m a
retryOnAnyError label { timeout, delay } action =
  recovering
    (limitRetriesByCumulativeDelay timeout $ constantDelay delay)
    [ \_ _ -> pure true ]
    ( \_ ->
        try action >>=
          case _ of
            Right x -> pure x
            Left err -> do
              logWarn' $
                "retryOnAnyError: action with label "
                  <> label
                  <> " has failed with error: "
                  <> Error.message err
              throwError err
    )
