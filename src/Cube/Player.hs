-- |

module Cube.Player
  ( Player(..)
  ) where

import Linear

data Player = Player { playerPos :: V3 Float
                     , playerRotation :: Quaternion Float
                     }
