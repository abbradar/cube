-- | Time utils.

{-# LANGUAGE StrictData #-}

module Cube.Time
  ( SDL.Timestamp
  , TicksElapsed
  , TimeInterval
  , isEarlier
  , isLater
  , earlier
  , later
  , intervalSeconds
  , secondsInterval
  , TimeStep(..)
  ) where

import Data.Int
import Data.Word
import SDL (Timestamp)

type TimeInterval = Word32
type TicksElapsed = TimeInterval

isEarlier :: Timestamp -> Timestamp -> Bool
a `isEarlier` b = diff >= 0
  where diff :: Int32
        diff = fromIntegral b - fromIntegral a

isLater :: Timestamp -> Timestamp -> Bool
a `isLater` b = diff >= 0
  where diff :: Int32
        diff = fromIntegral a - fromIntegral b

earlier :: Timestamp -> Timestamp -> Timestamp
earlier a b = if a `isEarlier` b then a else b

later :: Timestamp -> Timestamp -> Timestamp
later a b = if a `isLater` b then a else b

intervalSeconds :: Fractional a => TimeInterval -> a
intervalSeconds = (/ 1000) . fromIntegral

secondsInterval :: RealFrac a => a -> TimeInterval
secondsInterval = floor . (* 1000)

data TimeStep = TimeStep { ticksElapsed :: TicksElapsed
                         , currentTime :: Timestamp
                         }
              deriving (Show, Eq)
