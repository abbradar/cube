-- | Translate-Rotation-Scale transformations.

module Cube.Graphics.TRS
  ( TRS(..)
  , matrixToTRS
  , trsToMatrix
  ) where

import Control.Lens
import Linear

import Cube.Graphics.Geometry

data TRS a = TRS { trsTranslation :: V3 a
                 , trsRotation :: Quaternion a
                 , trsScale :: V3 a
                 }
           deriving (Show, Eq)

instance (Conjugate a, RealFloat a) => Semigroup (TRS a) where
  a <> b = TRS { trsTranslation = (trsTranslation a) + rotate (trsRotation a) (trsTranslation b)
               , trsRotation = trsRotation a * trsRotation b
               , trsScale = trsScale a * trsScale b
               }

instance (Conjugate a, RealFloat a) => Monoid (TRS a) where
  mempty = TRS { trsTranslation = 0
               , trsRotation = Quaternion 1 0
               , trsScale = 1
               }

matrixToTRS :: (Ord a, Floating a) => M44 a -> TRS a
matrixToTRS mtx = TRS {..}
  where trsTranslation = mtx ^. translation
        getScale col = norm $ mtx ^. column col . _xyz
        trsScale = V3 (getScale _x) (getScale _y) (getScale _z)
        rotationMtx = (mtx ^. _m33) / (V3 trsScale trsScale trsScale)
        trsRotation = matrixToQuaternion rotationMtx

-- FIXME: slow, but too lazy to implement efficiently.
trsToMatrix :: (Conjugate a, RealFloat a) => TRS a -> M44 a
trsToMatrix (TRS {..}) = transRot & _m33 *~ V3 trsScale trsScale trsScale
  where transRot = mkTransformation trsRotation trsTranslation
