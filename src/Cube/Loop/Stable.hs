-- | Stable event loop (with constant world time-step).

{-# LANGUAGE StrictData #-}

module Cube.Loop.Stable
  ( TicksElapsed
  , TimeInterval
  , EventLoop
  , CubeTickInfo(..)
  , newEventLoop
  , stepWithEvents
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Data.Int
import Data.Word
import qualified SDL

type TicksElapsed = Word32
type TimeInterval = Word32

data EventLoopState = EventLoopState { stateLastTime :: SDL.Timestamp
                                     , stateLastStepOffset :: TicksElapsed
                                     }

data EventLoop m = EventLoop { loopLastState :: IORef EventLoopState
                             , loopTickEvent :: CubeTickInfo -> m ()
                             , loopSDLEvent :: SDL.EventPayload -> m ()
                             }

newtype CubeTickInfo = CubeTickInfo { ticksElapsed :: TicksElapsed }
                     deriving (Show, Eq)

newEventLoop :: MonadIO m => (CubeTickInfo -> m ()) -> (SDL.EventPayload -> m ()) -> SDL.Timestamp -> m (EventLoop m)
newEventLoop loopTickEvent loopSDLEvent lastTime = do
  let state = EventLoopState { stateLastTime = lastTime
                             , stateLastStepOffset = 0
                             }
  loopLastState <- liftIO $ newIORef state
  return EventLoop {..}

stepWithEvents :: forall m. MonadIO m => EventLoop m -> SDL.Timestamp -> TimeInterval -> m ()
stepWithEvents  (EventLoop {..}) currentTime stepInterval = do
  state <- liftIO $ readIORef loopLastState
  events <- liftIO SDL.pollEvents
  state' <- foldM performSteps state events
  -- Perform last step.
  let rawElapsed = fromIntegral currentTime - fromIntegral (stateLastTime state') :: Int32
      (newCurrentTime, elapsed)
        | rawElapsed > 0 = (currentTime, rawElapsed)
        | otherwise = (stateLastTime state', 0)
  newOffset <- fireTicks (stateLastStepOffset state') elapsed
  let state'' = state' { stateLastTime = newCurrentTime, stateLastStepOffset = newOffset }
  liftIO $ writeIORef loopLastState state''

  where performSteps :: EventLoopState -> SDL.Event -> m EventLoopState
        performSteps state (SDL.Event {..}) = do
          newOffset <- fireTicks (stateLastStepOffset state) elapsed
          loopSDLEvent eventPayload
          return $ state { stateLastTime = eventTimestamp, stateLastStepOffset = newOffset }
          where rawElapsed = fromIntegral eventTimestamp - fromIntegral (stateLastTime state) :: Int32
                elapsed
                  | rawElapsed > 0 = rawElapsed
                  | otherwise = 0

        tickInfo = CubeTickInfo { ticksElapsed = stepInterval }

        fireTicks :: SDL.Timestamp -> Int32 -> m SDL.Timestamp
        fireTicks initialOffset rawElapsed = go (initialOffset + fromIntegral rawElapsed)

          where go offset
                  | offset < stepInterval = return offset
                  | otherwise = do
                      loopTickEvent tickInfo
                      go (offset - stepInterval)
