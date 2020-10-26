-- | Extra instances for Reflex typeclasses.

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE UndecidableInstances #-}

module Reflex.Logger where

import Control.Monad.Trans.Class
import Control.Monad.Logger
import Reflex
import Reflex.Host.Class

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (LoggingT m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger initializer = lift $ newFanEventWithTrigger initializer

instance MonadSubscribeEvent t m => MonadSubscribeEvent t (LoggingT m) where
  subscribeEvent = lift . subscribeEvent

instance MonadReflexHost t m => MonadReflexHost t (LoggingT m) where
  type ReadPhase (LoggingT m) = ReadPhase m

  fireEventsAndRead dm a = lift $ fireEventsAndRead dm a
  runHostFrame = lift . runHostFrame

instance MonadSample t m => MonadSample t (LoggingT m) where
  sample = lift . sample
