-- | Extra instances for Reflex typeclasses.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Logger where

import Control.Monad.Fix
import Control.Monad.Trans.Class
import Control.Monad.Logger
import Reflex
import Reflex.Host.Class

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggingT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LoggingT m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReadEvent t m => MonadReadEvent t (LoggingT m) where
  readEvent = lift . fmap (fmap lift) . readEvent

instance MonadReflexHost t m => MonadReflexHost t (LoggingT m) where
  type ReadPhase (LoggingT m) = ReadPhase m

  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance MonadSample t m => MonadSample t (LoggingT m) where
  sample = lift . sample

instance MonadHold t m => MonadHold t (LoggingT m) where
  hold a ev = lift $ hold a ev
  holdDyn a ev = lift $ holdDyn a ev
  holdIncremental target ev = lift $ holdIncremental target ev
  buildDynamic comp ev = lift $ buildDynamic comp ev
  headE ev = lift $ headE ev

instance MonadFix m => MonadFix (LoggingT m) where
    mfix f = LoggingT $ \r -> mfix $ \a -> runLoggingT (f a) r
