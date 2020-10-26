-- | Cameras with projection.

module Cube.Graphics.Camera
  ( Camera(..)
  , cameraToMatrix
  ) where

import Linear

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

cameraToMatrix :: Floating a => Camera a -> M44 a
cameraToMatrix (Camera {..}) = mkTransformation cameraRotation cameraPosition
