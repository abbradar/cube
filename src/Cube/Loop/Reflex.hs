-- | Bindings for Reflex.

{-# LANGUAGE StrictData #-}

module Cube.Loop.Reflex
  ( ReflexEventLoop
  , newReflexEventLoop
  , EventLoopApp(..)
  , runInEventLoop
  ) where

import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Logger
import qualified SDL
import Reflex
import Reflex.Host.Class

import Reflex.Logger ()
import Reflex.Spider.Catch ()
import Control.Monad.Ref.Logger ()
import Cube.Types
import Cube.Loop.Stable
import Cube.Graphics.Framerate

data ReflexEventLoop t m = ReflexEventLoop { reflexEventLoop :: EventLoop m
                                           , reflexTickEvent :: Event t CubeTickInfo
                                           , reflexSDLEvent :: Event t SDL.EventPayload
                                           }

newReflexEventLoop :: (MonadReflexCreateTrigger t m, MonadReflexHost t m, Reflex t, MonadIO m, MonadRef m, Ref m ~ Ref IO) => SDL.Timestamp -> m (ReflexEventLoop t m)
newReflexEventLoop lastTime = do
  (reflexTickEvent, tickRef) <- newEventWithTriggerRef
  (reflexSDLEvent, sdlRef) <- newEventWithTriggerRef
  reflexEventLoop <- newEventLoop (fireEventRef tickRef) (fireEventRef sdlRef) lastTime
  return ReflexEventLoop {..}

data EventLoopApp a = EventLoopApp { appNetworkSetup :: forall t m. (Reflex t, MonadCube m) => Event t CubeTickInfo -> Event t SDL.EventPayload -> m (Behavior t (Maybe a))
                                   , appDrawFrame :: forall m. (MonadCube m) => a -> m ()
                                   , appFrameInterval :: TimeInterval
                                   , appWorldInterval :: TimeInterval
                                   }

runInEventLoop :: MonadCube m => EventLoopApp a -> m ()
runInEventLoop (EventLoopApp {..}) = do
  logger <- askLoggerIO
  liftIO $ runSpiderHost $ flip runLoggingT logger $ do
    (initialTicks, fpsLimit) <- newFPSLimit
    ReflexEventLoop {..} <- newReflexEventLoop initialTicks
    behavior <- runHostFrame $ flip runLoggingT logger $ appNetworkSetup reflexTickEvent reflexSDLEvent

    let go currentTime = do
          stepWithEvents reflexEventLoop currentTime appWorldInterval
          mstate <- sample behavior
          case mstate of
            Nothing -> return ()
            Just state -> do
              appDrawFrame state
              (newTime, _elapsed) <- fpsDelay fpsLimit appFrameInterval
              go newTime

    go initialTicks
