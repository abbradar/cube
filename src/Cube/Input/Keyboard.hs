-- | Transform from raw SDL events to direction vectors.

module Cube.Input.Keyboard
  ( PressedKeys(..)
  , pressedSDLKeys
  , moveDirection
  , keysDirection
  ) where

import Control.Monad.Fix
import Linear
import Data.Set (Set)
import qualified Data.Set as S
import Reflex

import SDL hiding (Event)

newtype PressedKeys = PressedKeys { pressedKeys :: Set Keycode }
                    deriving newtype (Show, Eq, Semigroup, Monoid)

pressedSDLKeys :: (Reflex t, MonadFix m, MonadHold t m) => Event t EventPayload -> m (Dynamic t PressedKeys)
pressedSDLKeys = foldDyn process mempty
  where process (KeyboardEvent (KeyboardEventData { keyboardEventKeyMotion = motion, keyboardEventKeysym = Keysym { keysymKeycode = kc } })) (PressedKeys {..}) =
          case motion of
            Pressed -> PressedKeys $ S.insert kc pressedKeys
            Released -> PressedKeys $ S.delete kc pressedKeys
        process _ keys = keys

moveDirection :: (Epsilon a, Floating a) => PressedKeys -> V3 a
moveDirection (PressedKeys {..}) = normalize $ forward + back + left + right + up + down
  where forward = if S.member KeycodeW pressedKeys then V3 1 0 0 else 0
        back = if S.member KeycodeS pressedKeys then V3 (-1) 0 0 else 0
        left = if S.member KeycodeA pressedKeys then V3 0 (-1) 0 else 0
        right = if S.member KeycodeD pressedKeys then V3 0 1 0 else 0
        up = if S.member KeycodeSpace pressedKeys then V3 0 0 1 else 0
        down = if S.member KeycodeLCtrl pressedKeys then V3 0 0 (-1) else 0

keysDirection :: (Reflex t, Epsilon a, Floating a, MonadFix m, MonadHold t m) => Dynamic t PressedKeys -> m (Dynamic t (V3 a))
keysDirection = scanDyn moveDirection (\keys _ -> moveDirection keys)
