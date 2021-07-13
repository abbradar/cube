-- | Stable event loop (with constant world time-step).

{-# LANGUAGE StrictData #-}

module Cube.Loop.Stable
  ( EventLoop
  , CubeTickInfo(..)
  , newEventLoop
  , EventLoopHooks(..)
  , stepWithEvents
  ) where

import Data.Maybe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.IORef
import qualified SDL

import Cube.Time

data EventLoopState = EventLoopState { lastStepTime :: SDL.Timestamp
                                     , lastEventTime :: SDL.Timestamp
                                     }
                    deriving (Show, Eq)

data EventLoop = EventLoop { loopLastState :: IORef (Maybe EventLoopState)
                           }

data CubeTickInfo = CubeTickInfo { ticksElapsed :: TicksElapsed
                                 , currentTime :: SDL.Timestamp
                                 }
                     deriving (Show, Eq)

newEventLoop :: MonadIO m => SDL.Timestamp -> m EventLoop
newEventLoop lastTime = do
  let state = EventLoopState { lastStepTime = lastTime
                             , lastEventTime = lastTime
                             }
  loopLastState <- liftIO $ newIORef $ Just state
  return EventLoop {..}

data EventLoopHooks m = EventLoopHooks { loopTickEvent :: CubeTickInfo -> m Bool
                                       , loopSDLEvent :: CubeTickInfo -> SDL.EventPayload -> m Bool
                                       }

stepWithEvents :: forall m. MonadIO m => EventLoop -> EventLoopHooks m -> SDL.Timestamp -> TimeInterval -> m Bool
stepWithEvents  (EventLoop {..}) (EventLoopHooks {..}) currentTime stepInterval = do
  mstate <- liftIO $ readIORef loopLastState
  case mstate of
    Nothing -> return False
    Just state -> do
      events <- liftIO SDL.pollEvents
      r <- runMaybeT $ do
        state' <- foldM handleEvent state events
        if currentTime `isLater` lastEventTime state' then
          performSteps state' currentTime
        else
          return state'
      liftIO $ writeIORef loopLastState r
      return $ isJust r

  where handleEvent :: EventLoopState -> SDL.Event -> MaybeT m EventLoopState
        handleEvent state (SDL.Event {..}) = do
          -- Last event time can be later than current event time due to race between `newEventLoop` and events happening.
          let eventTimestamp' = later eventTimestamp (lastEventTime state)
          state' <- performSteps state eventTimestamp'
          let tickInfo = CubeTickInfo { ticksElapsed = eventTimestamp' - lastEventTime state'
                                      , currentTime = eventTimestamp'
                                      }
          r <- lift $ loopSDLEvent tickInfo eventPayload
          guard r
          return $ state' { lastEventTime = eventTimestamp' }

        performSteps :: EventLoopState -> SDL.Timestamp -> MaybeT m EventLoopState
        performSteps state eventTime
          | eventTime - lastStepTime state < stepInterval = return state
          | otherwise = do
              let stepTime = lastStepTime state + stepInterval
                  tickInfo = CubeTickInfo { ticksElapsed = eventTime - lastEventTime state
                                          , currentTime = stepTime
                                          }
              r <- lift $ loopTickEvent tickInfo
              guard r
              let state' = state { lastStepTime = lastStepTime state + stepInterval, lastEventTime = eventTime }
              performSteps state' eventTime
