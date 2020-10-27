-- | Stable event loop (with constant world time-step).

{-# LANGUAGE StrictData #-}

module Cube.Loop.Stable
  ( TicksElapsed
  , TimeInterval
  , EventLoop
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
import Data.Int
import Data.Word
import qualified SDL

type TicksElapsed = Word32
type TimeInterval = Word32

data EventLoopState = EventLoopState { stateLastTime :: SDL.Timestamp
                                     , stateLastStepOffset :: TicksElapsed
                                     }

data EventLoop = EventLoop { loopLastState :: IORef EventLoopState
                           }

newtype CubeTickInfo = CubeTickInfo { ticksElapsed :: TicksElapsed }
                     deriving (Show, Eq)

newEventLoop :: MonadIO m => SDL.Timestamp -> m EventLoop
newEventLoop lastTime = do
  let state = EventLoopState { stateLastTime = lastTime
                             , stateLastStepOffset = 0
                             }
  loopLastState <- liftIO $ newIORef state
  return EventLoop {..}

data EventLoopHooks m = EventLoopHooks { loopTickEvent :: CubeTickInfo -> m Bool
                                       , loopSDLEvent :: SDL.EventPayload -> m Bool
                                       }

stepWithEvents :: forall m. MonadIO m => EventLoop -> EventLoopHooks m -> SDL.Timestamp -> TimeInterval -> m Bool
stepWithEvents  (EventLoop {..}) (EventLoopHooks {..}) currentTime stepInterval = do
  state <- liftIO $ readIORef loopLastState
  events <- liftIO SDL.pollEvents
  r <- runMaybeT $ do
    state' <- foldM performSteps state events
    -- Perform last step.
    let rawElapsed = fromIntegral currentTime - fromIntegral (stateLastTime state') :: Int32
        (newCurrentTime, elapsed)
          | rawElapsed > 0 = (currentTime, rawElapsed)
          | otherwise = (stateLastTime state', 0)
    newOffset <- fireTicks (stateLastStepOffset state') elapsed
    let state'' = state' { stateLastTime = newCurrentTime, stateLastStepOffset = newOffset }
    liftIO $ writeIORef loopLastState state''
  return $ if isJust r then True else False

  where performSteps :: EventLoopState -> SDL.Event -> MaybeT m EventLoopState
        performSteps state (SDL.Event {..}) = do
          newOffset <- fireTicks (stateLastStepOffset state) elapsed
          r <- lift $ loopSDLEvent eventPayload
          case eventPayload of
            _ | not r -> mzero
            SDL.QuitEvent -> mzero
            _ -> return $ state { stateLastTime = eventTimestamp, stateLastStepOffset = newOffset }
          where rawElapsed = fromIntegral eventTimestamp - fromIntegral (stateLastTime state) :: Int32
                elapsed
                  | rawElapsed > 0 = rawElapsed
                  | otherwise = 0

        tickInfo = CubeTickInfo { ticksElapsed = stepInterval }

        fireTicks :: SDL.Timestamp -> Int32 -> MaybeT m SDL.Timestamp
        fireTicks initialOffset rawElapsed = go (initialOffset + fromIntegral rawElapsed)

          where go offset
                  | offset < stepInterval = return offset
                  | otherwise = do
                      r <- lift $ loopTickEvent tickInfo
                      if r then go (offset - stepInterval) else mzero
