-- | Read mouse inputs from raw SDL events.

module Cube.Input.Mouse
  ( relativeMovePerTick
  ) where

import Data.Maybe
import Data.Int
import Control.Monad.Fix
import Linear
import Reflex
import SDL hiding (Event)

import Cube.Time
import Cube.Input.Accumulate

relativeMovePerTick :: (Reflex t, MonadFix m, MonadHold t m) => Event t TimeStep -> Event t (TimeStep, MouseMotionEventData) -> m (Event t (V2 Int32))
relativeMovePerTick tickEvent mEvent = accumulateMovement tickEvent (fmapMaybe getShift mEvent)
  where getShift (_tick, info)
          | isJust $ mouseMotionEventWindow info = Just $ mouseMotionEventRelMotion info
          | otherwise = Nothing
