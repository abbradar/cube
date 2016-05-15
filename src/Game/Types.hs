module Game.Types where

import Data.Int
import Data.Word
import Data.Set (Set)
import SDL.Input.Keyboard.Codes
import Linear
import Control.Lens.TH

import Engine.Camera

data GameState = GameState { _camera :: Camera
                           , _pressedKeys :: Set Keycode
                           , _movedMouse :: V2 Int32
                           , _frameSize :: V2 Int32
                           , _frameTime :: Word32
                           }

$(makeLenses ''GameState)
