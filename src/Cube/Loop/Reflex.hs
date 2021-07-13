-- | Bindings for Reflex, providing a complete event loop. Builts atop `Cube.Loop.Stable`.

{-# LANGUAGE StrictData #-}

module Cube.Loop.Reflex
  ( ReflexEventLoop
  , reflexTickEvent
  , reflexSDLEvent
  , newReflexEventLoop
  , stepWithReflexEvents
  , EventLoopNetwork(..)
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
import Reflex.Catch ()
import Control.Monad.Ref.Logger ()
import Cube.Types
import Cube.Time
import Cube.Loop.Stable
import Cube.Graphics.Framerate

data ReflexEventLoop t = ReflexEventLoop { reflexEventLoop :: EventLoop
                                         , reflexTickEvent :: Event t CubeTickInfo
                                         , reflexTickRef :: IORef (Maybe (EventTrigger t CubeTickInfo))
                                         , reflexSDLEvent :: Event t (CubeTickInfo, SDL.EventPayload)
                                         , reflexSDLRef :: IORef (Maybe (EventTrigger t (CubeTickInfo, SDL.EventPayload)))
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

stepWithReflexEvents :: forall t m res. (MonadReflexHost t m, MonadIO m, MonadRef m, Ref m ~ Ref IO) => ReflexEventLoop t -> ReadPhase m res -> (res -> m Bool) -> SDL.Timestamp -> TimeInterval -> m Bool
stepWithReflexEvents (ReflexEventLoop {..}) readPhase interpretPhase = stepWithEvents reflexEventLoop hooks
  where hooks = EventLoopHooks { loopTickEvent = runEvent reflexTickRef
                               , loopSDLEvent = curry $ runEvent reflexSDLRef
                               }
        runEvent :: Ref m (Maybe (EventTrigger t e)) -> e -> m Bool
        runEvent ref event = do
          r <- fireEventRefAndRead' ref event readPhase
          fromMaybe True <$> mapM interpretPhase r

data EventLoopNetwork t frame = EventLoopNetwork { eloopFrameBehavior :: Behavior t frame
                                                 , eloopQuitEvent :: Event t ()
                                                 }

data EventLoopApp frame extra action =
  EventLoopApp { eappNetworkSetup :: forall t m. (Reflex t, MonadHold t m, MonadSample t m, MonadCube m, MonadSubscribeEvent t m) => Event t CubeTickInfo -> Event t (CubeTickInfo, SDL.EventPayload) -> m (extra t, EventLoopNetwork t frame)
               , eappDrawFrame :: forall t m. (MonadCube m) => extra t -> frame -> m ()
               , eappReadAction :: forall t m. (MonadReadEvent t m) => extra t -> m action
               , eappInterpretAction :: forall m. (MonadCube m) => action -> m ()
               , eappFrameInterval :: TimeInterval
               , eappWorldInterval :: TimeInterval
               }

runInEventLoop :: MonadCube m => EventLoopApp frame action actionResult -> m ()
runInEventLoop (EventLoopApp {..}) = do
  logger <- askLoggerIO
  liftIO $ runSpiderHost $ do
    (initialTicks, fpsLimit) <- newFPSLimit
    eventLoop <- newReflexEventLoop initialTicks
    (extraInfo, EventLoopNetwork {..}) <- runHostFrame $ flip runLoggingT logger $ eappNetworkSetup (reflexTickEvent eventLoop) (reflexSDLEvent eventLoop)
    quitHandle <- subscribeEvent eloopQuitEvent

    let go currentTime = do
          let myReadPhase = do
                action <- eappReadAction extraInfo
                quitHappened <- readEvent quitHandle
                return (action, isNothing quitHappened)
          let myInterpretPhase (action, continue) = do
                eappInterpretAction action
                return continue
          r <- stepWithReflexEvents eventLoop myReadPhase myInterpretPhase currentTime eappWorldInterval
          if not r then
            return ()
          else do
            state <- sample eloopFrameBehavior
            eappDrawFrame extraInfo state
            (newTime, _elapsed) <- fpsDelay fpsLimit eappFrameInterval
            go newTime

    flip runLoggingT logger $ go initialTicks
