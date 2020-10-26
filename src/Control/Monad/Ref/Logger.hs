-- | MonadRef instances for LoggingT.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.Monad.Ref.Logger where

import Control.Monad.Trans.Class
import Control.Monad.Ref
import Control.Monad.Logger

instance MonadRef m => MonadRef (LoggingT m) where
  type Ref (LoggingT m) = Ref m

  newRef = lift . newRef
  readRef = lift . readRef
  writeRef ref a = lift $ writeRef ref a
  modifyRef ref f = lift $ modifyRef ref f
  modifyRef' ref f = lift $ modifyRef' ref f

instance MonadAtomicRef m => MonadAtomicRef (LoggingT m) where
  atomicModifyRef ref f = lift $ atomicModifyRef ref f
  atomicModifyRef' ref f = lift $ atomicModifyRef' ref f
