-- | Bindings for Reflex.

{-# LANGUAGE StrictData #-}

module Cube.Loop.Reflex
  ( ReflexEventLoop
  , reflexTickEvent
  , reflexSDLEvent
  , newReflexEventLoop
  , stepWithReflexEvents
  , CubeNetwork(..)
  , EventLoopApp(..)
  , runInEventLoop
  ) where

import Data.Maybe
import Data.IORef
import Control.Monad.IO.Class
import Control.Monad.Ref
import Control.Monad.Logger
import Data.Dependent.Sum
import Data.Functor.Identity
import qualified SDL
import Reflex
import Reflex.Host.Class

import Reflex.Logger ()
import Reflex.Spider.Catch ()
import Control.Monad.Ref.Logger ()
import Cube.Types
import Cube.Loop.Stable
import Cube.Graphics.Framerate

data ReflexEventLoop t = ReflexEventLoop { reflexEventLoop :: EventLoop
                                         , reflexTickEvent :: Event t CubeTickInfo
                                         , reflexTickRef :: IORef (Maybe (EventTrigger t CubeTickInfo))
                                         , reflexSDLEvent :: Event t SDL.EventPayload
                                         , reflexSDLRef :: IORef (Maybe (EventTrigger t SDL.EventPayload))
                                         }

newReflexEventLoop :: (MonadReflexCreateTrigger t m, MonadIO m, MonadRef m, Ref m ~ Ref IO) => SDL.Timestamp -> m (ReflexEventLoop t)
newReflexEventLoop lastTime = do
  (reflexTickEvent, reflexTickRef) <- newEventWithTriggerRef
  (reflexSDLEvent, reflexSDLRef) <- newEventWithTriggerRef
  reflexEventLoop <- newEventLoop lastTime
  return ReflexEventLoop {..}

fireEventRefAndRead' :: (MonadReflexHost t m, MonadRef m, Ref m ~ Ref IO) => Ref m (Maybe (EventTrigger t a)) -> a -> ReadPhase m b -> m (Maybe b)
fireEventRefAndRead' mtRef input readPhase = do
  mt <- readRef mtRef
  case mt of
    Nothing -> return Nothing -- Since we aren't firing the input, the output can't fire
    Just trigger -> fireEventsAndRead [trigger :=> Identity input] (Just <$> readPhase)

stepWithReflexEvents :: (MonadReflexHost t m, MonadIO m, MonadRef m, Ref m ~ Ref IO) => ReflexEventLoop t -> ReadPhase m Bool -> SDL.Timestamp -> TimeInterval -> m Bool
stepWithReflexEvents (ReflexEventLoop {..}) readPhase = stepWithEvents reflexEventLoop hooks
  where hooks = EventLoopHooks { loopTickEvent = \i -> fromMaybe True <$> fireEventRefAndRead' reflexTickRef i readPhase
                               , loopSDLEvent = \i -> fromMaybe True <$> fireEventRefAndRead' reflexSDLRef i readPhase
                               }

data CubeNetwork t a = CubeNetwork { frameBehavior :: Behavior t a
                                   , quitEvent :: Event t ()
                                   }

data EventLoopApp a = EventLoopApp { appNetworkSetup :: forall t m. (Reflex t, MonadHold t m, MonadSample t m, MonadCube m) => Event t CubeTickInfo -> Event t SDL.EventPayload -> m (CubeNetwork t a)
                                   , appDrawFrame :: forall m. (MonadCube m) => a -> m ()
                                   , appFrameInterval :: TimeInterval
                                   , appWorldInterval :: TimeInterval
                                   }

runInEventLoop :: forall m a. MonadCube m => EventLoopApp a -> m ()
runInEventLoop (EventLoopApp {..}) = do
  logger <- askLoggerIO
  liftIO $ runSpiderHost $ do
    (initialTicks, fpsLimit) <- newFPSLimit
    eventLoop <- newReflexEventLoop initialTicks
    CubeNetwork {..} <- runHostFrame $ flip runLoggingT logger $ appNetworkSetup (reflexTickEvent eventLoop) (reflexSDLEvent eventLoop)
    quitHandle <- subscribeEvent quitEvent

    let go currentTime = do
          let readQuit = do
                shouldQuit <- readEvent quitHandle
                return $ isNothing shouldQuit
          r <- stepWithReflexEvents eventLoop readQuit currentTime appWorldInterval
          if not r then
            return ()
          else do
            state <- sample frameBehavior
            flip runLoggingT logger $ appDrawFrame state
            (newTime, _elapsed) <- fpsDelay fpsLimit appFrameInterval
            go newTime

    go initialTicks
