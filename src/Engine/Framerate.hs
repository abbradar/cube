module Engine.Framerate
       ( FPSLimit
       , newFPSLimit
       , fpsDelay
       ) where

import Data.Word
import Control.Concurrent (threadDelay)
import Data.IORef

import SDL.Time

-- | FPS limiter using SDL timer.
newtype FPSLimit = FPSLimit (IORef Word32)

-- | Create initial 'FPSLimit'.
newFPSLimit :: IO FPSLimit
newFPSLimit = do
  t0 <- ticks
  FPSLimit <$> newIORef t0

-- | Given frame time (inverted FPS), delay a thread.
fpsDelay :: FPSLimit -> Word32 -> IO ()
fpsDelay (FPSLimit st) limit = do
  old <- readIORef st
  curr <- ticks
  let target = old + limit
  let diff = target - curr
  next <- if diff < maxBound `div` 2
          then do
            threadDelay $ 1000 * fromIntegral diff
            return target
          else return curr
  writeIORef st next
