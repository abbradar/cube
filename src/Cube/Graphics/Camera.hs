-- | Cameras with projection.

module Cube.Graphics.Camera
  ( Camera(..)
  , FloatingCamera(..)
  , fCameraToMatrix
  , fCameraLookAt
  , fCameraRotateNoRoll
  , fCameraPosition
  , cameraToMatrix
  , cameraFromEyeTarget
  , cameraRotateNoRoll
  ) where

import Control.Lens
import Linear

import Cube.Graphics.Geometry

defaultUp :: Num a => V3 a
defaultUp = V3 0 0 1

data FloatingCamera a = FloatingCamera { fCameraTarget :: V3 a
                                       , fCameraDistance :: a
                                       , fCameraRotation :: Quaternion a
                                       }
                        deriving (Show, Eq)

fCameraLookAt :: (Show a, Floating a, Ord a, Epsilon a) => V3 a -> V3 a -> FloatingCamera a
fCameraLookAt eye target =
  FloatingCamera { fCameraTarget = target
                 , fCameraDistance = norm (target - eye)
                 , fCameraRotation = matrixToQuaternion $ lookAt eye target defaultUp ^. _m33
                 }

fCameraToMatrix :: (Conjugate a, RealFloat a, Show a) => FloatingCamera a -> M44 a
fCameraToMatrix (FloatingCamera {..}) = inv44 $ mkTransformation fCameraRotation (fCameraTarget + rotate fCameraRotation (V3 0 0 fCameraDistance))

fCameraPosition :: (Conjugate a, RealFloat a, Show a) => FloatingCamera a -> V3 a
fCameraPosition FloatingCamera{..} = V3 0 0 fCameraDistance

fCameraRotateNoRoll :: (Conjugate a, RealFloat a, Epsilon a) => V2 a -> FloatingCamera a -> FloatingCamera a
fCameraRotateNoRoll (V2 x y) camera =
  camera { fCameraRotation = rotUp * fCameraRotation camera * rotRight }
  where rotRight = axisAngle (V3 1 0 0) (-y)
        rotUp = axisAngle (V3 0 0 1) (-x)

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

cameraToMatrix :: (Conjugate a, RealFloat a, Show a) => Camera a -> M44 a
cameraToMatrix (Camera {..}) = inv44 $ mkTransformation cameraRotation cameraPosition

cameraRotateNoRoll :: (Conjugate a, RealFloat a, Epsilon a) => V2 a -> Camera a -> Camera a
cameraRotateNoRoll (V2 x y) camera =
  camera { cameraRotation = rotUp * cameraRotation camera * rotRight }
  where rotRight = axisAngle (V3 1 0 0) (-y)
        rotUp = axisAngle (V3 0 0 1) (-x)
