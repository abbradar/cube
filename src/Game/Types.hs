module Game.Types where

import Data.Int
import Data.Word
import Data.Bool
import Data.Set (Set)
import SDL.Input.Keyboard.Codes
import Linear
import Control.Lens.TH

import Engine.Camera
piska

data GameState = GameState { _camera :: Camera
                           , _pressedKeys :: Set Keycode
                           , _movedMouse :: V2 Int32
                           , _leftButton :: Bool
                           , _frameSize :: V2 Int32
                           , _frameTime :: Word32
                           }

$(makeLenses ''GameState)
