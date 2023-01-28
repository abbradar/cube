-- |

module Cube.Player
  ( Player(..)
  ) where

import Linear
import Cube.Map


data Player = Player { playerPos :: V3 Float
                     , playerRotation :: Quaternion Float
                     }

instance Movable Player where
  moveRotate pl@Player{..} shift rotation _ = pl{ playerPos = playerPos + shift, playerRotation = rotation * playerRotation }
