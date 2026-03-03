module CardanoRacers.Hydra.Lib.AVar
  ( readNow
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Effect.Aff.AVar (AVar)
import Effect.Aff.AVar (tryRead) as AVar
import Effect.Aff.Class (class MonadAff, liftAff)

readNow
  :: forall (m :: Type -> Type) (e :: Type) (a :: Type)
   . MonadAff m
  => MonadThrow e m
  => e
  -> AVar a
  -> m a
readNow err avar =
  liftMaybe err
    =<< liftAff (AVar.tryRead avar)
