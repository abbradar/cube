-- | Transform from raw SDL events to direction vectors.

module Cube.Input.Keyboard
  ( pressedSDLKeys
  , moveDirection
  ) where

import Witherable
import Control.Monad.Fix
import Linear
import qualified Data.Map.Strict as M
import Reflex
import SDL hiding (Event)

import Cube.Loop.Stable (CubeTickInfo(..))
import Cube.Input.Accumulate

pressedSDLKeys :: (Reflex t, MonadFix m, MonadHold t m) => Event t CubeTickInfo -> Event t (CubeTickInfo, EventPayload) -> m (Event t (AccumulatedInput Keycode))
pressedSDLKeys tickEvent sdlEvent = accumulateInput tickEvent (mapMaybe getKeyboardEvent sdlEvent)
  where getKeyboardEvent (tick, event) =
          case event of
            KeyboardEvent (KeyboardEventData { keyboardEventKeyMotion = motion, keyboardEventKeysym = Keysym { keysymKeycode = kc } }) ->
              let isPressed =
                    case motion of
                      Pressed -> True
                      Released -> False
              in Just (tick, (kc, isPressed))
            _ -> Nothing

moveDirection :: (Epsilon a, Floating a) => AccumulatedInput Keycode -> V3 a
moveDirection pressedKeys = normalize $ forward + back + left + right + up + down
  where checkKey keycode vec =
          case M.lookup keycode pressedKeys of
            Nothing -> 0
            Just events -> fromIntegral (sumInputTime events) *^ vec

        forward = checkKey KeycodeW (V3 1 0 0)
        back = checkKey KeycodeS (V3 (-1) 0 0)
        left = checkKey KeycodeA (V3 0 (-1) 0)
        right = checkKey KeycodeD (V3 0 1 0)
        up = checkKey KeycodeSpace (V3 0 0 1)
        down = checkKey KeycodeLCtrl (V3 0 0 (-1))
