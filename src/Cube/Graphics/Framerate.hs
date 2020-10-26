{-# LANGUAGE StrictData #-}

module Cube.Graphics.Framerate
       ( FPSLimit
       , newFPSLimit
       , fpsDelay
       ) where

import Control.Monad.IO.Class
import Data.Int
import Data.Word
import Control.Concurrent (threadDelay)
import Data.IORef

import qualified SDL

-- | FPS limiter using SDL timer.
newtype FPSLimit = FPSLimit (IORef Word32)

-- | Create initial 'FPSLimit'.
newFPSLimit :: MonadIO m => m (SDL.Timestamp, FPSLimit)
newFPSLimit = liftIO $ do
  t0 <- SDL.ticks
  r <- FPSLimit <$> newIORef t0
  return (t0, r)

-- | Given frame time (inverted FPS), delay a thread. Return approximate current time and last frame time.
fpsDelay :: MonadIO m => FPSLimit -> Word32 -> m (SDL.Timestamp, Word32)
fpsDelay (FPSLimit st) limit = liftIO $ do
  old <- readIORef st
  curr <- SDL.ticks
  let target = old + limit
  let diff = fromIntegral target - fromIntegral curr :: Int32
  (next, interval) <-
    if diff > 0
    then do
      threadDelay $ 1000 * fromIntegral diff
      return (target, limit)
    else return (curr, curr - old)
  writeIORef st next
  return (next, interval)
