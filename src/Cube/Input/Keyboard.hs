-- | Transform from raw SDL events to direction vectors.

module Cube.Input.Keyboard
  ( pressedKeyPerTick
  , normalizedMove
  ) where

import Data.Functor.Misc
import Control.Monad.Fix
import Linear
import Reflex
import SDL hiding (Event)

import Cube.Time
import Cube.Input.Events
import Cube.Input.Accumulate

-- This function doesn't check
pressedKeyPerTick :: (Reflex t, MonadFix m, MonadHold t m) => Keycode -> Event t TimeStep -> KeyboardEventSelector t -> m (Event t TicksElapsed)
pressedKeyPerTick keycode tickEvent kbEvents = accumulatePresses tickEvent (fmap getPressed $ select kbEvents (Const2 keycode))
  where getPressed (tick, info) =
          let isPressed =
                case keyboardEventKeyMotion info of
                  Pressed -> True
                  Released -> False
          in (tick, isPressed)

normalizedMove :: (Reflex t, Epsilon a, Floating a, MonadFix m, MonadHold t m) => Event t TimeStep -> KeyboardEventSelector t -> m (Event t (V3 a))
normalizedMove tickEvent kbEvents = do
  keyEvents <- mapM (uncurry eventForKey) keys
  return $ normalize <$> mergeWith (+) keyEvents

  where eventForKey keycode vec = do
          event <- pressedKeyPerTick keycode tickEvent kbEvents
          return $ fmap (\elapsed -> fromIntegral elapsed *^ vec) event

        keys = [ (KeycodeS, V3 0 0 1)
               , (KeycodeW, V3 0 0 (-1))
               , (KeycodeD, V3 1 0 0)
               , (KeycodeA, V3 (-1) 0 0)
               , (KeycodeLCtrl, V3 0 (-1) 0)
               , (KeycodeSpace, V3 0 1 0)
               ]
