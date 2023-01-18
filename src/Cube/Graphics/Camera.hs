-- | Cameras with projection.

module Cube.Graphics.Camera
  ( Camera(..)
  , cameraToMatrix
  , cameraFromEyeTarget
  , cameraRotateNoRoll
  ) where

import Control.Lens
import Linear

import Cube.Graphics.Geometry

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

cameraToMatrix :: (Conjugate a, RealFloat a) => Camera a -> M44 a
cameraToMatrix (Camera {..}) = inv44 $ mkTransformation cameraRotation cameraPosition

cameraRotateNoRoll :: (Conjugate a, RealFloat a, Epsilon a) => V2 a -> Camera a -> Camera a
cameraRotateNoRoll (V2 x y) camera =
  camera { cameraRotation = rotUp * cameraRotation camera * rotRight }
  where rotRight = axisAngle (V3 1 0 0) (-y)
        rotUp = axisAngle (V3 0 1 0) (-x)
