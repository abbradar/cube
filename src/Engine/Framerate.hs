module Engine.Framerate
       ( FPSLimit
       , newFPSLimit
       , fpsDelay
       , getTicks
       ) where

import Data.Word
import Control.Concurrent (threadDelay)
import Data.IORef

import SDL.Time

-- | FPS limiter using SDL timer.
newtype FPSLimit = FPSLimit (IORef Word32)

getTicks :: IO Word32
getTicks = ticks

-- | Create initial 'FPSLimit'.
newFPSLimit :: IO FPSLimit
newFPSLimit = do
  t0 <- ticks
  FPSLimit <$> newIORef t0

-- | Given frame time (inverted FPS), delay a thread.
fpsDelay :: FPSLimit -> Word32 -> IO Word32
fpsDelay (FPSLimit st) limit = do
  old <- readIORef st
  curr <- ticks
  let target = old + limit
  let diff = target - curr
  (next, interval) <-
    if diff < maxBound `div` 2
    then do
      threadDelay $ 1000 * fromIntegral diff
      return (target, limit)
    else return (curr, curr - old)
  writeIORef st next
  return interval
