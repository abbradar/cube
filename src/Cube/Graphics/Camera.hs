-- | Cameras with projection.

module Cube.Graphics.Camera
  ( Camera(..)
  , cameraToMatrix
  , cameraFromEyeTarget
  ) where

import Control.Lens
import Linear

import Cube.Graphics.TRS

defaultUp :: Num a => V3 a
defaultUp = V3 0 0 1

data Camera a = Camera { cameraPosition :: V3 a
                       , cameraRotation :: Quaternion a
                       }
              deriving (Show, Eq)

instance RealFloat a => Semigroup (Camera a) where
  a <> b = Camera { cameraPosition = cameraPosition a + cameraPosition b
                  , cameraRotation = cameraRotation a * cameraRotation b
                  }

instance RealFloat a => Monoid (Camera a) where
  mempty = Camera { cameraPosition = 0
                  , cameraRotation = Quaternion 1 0
                  }

cameraFromEyeTarget :: (Show a, Floating a, Ord a, Epsilon a) => V3 a -> V3 a -> Camera a
cameraFromEyeTarget eye target =
  Camera { cameraPosition = eye
         , cameraRotation = matrixToQuaternion $ lookAt eye target defaultUp ^. _m33
         }


cameraToMatrix :: Floating a => Camera a -> M44 a
cameraToMatrix (Camera {..}) = mkTransformation cameraRotation cameraPosition
