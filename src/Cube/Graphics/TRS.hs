-- | Translate-Rotation-Scale transformations.

module Cube.Graphics.TRS
  ( TRS(..)
  , matrixToQuaternion
  , matrixToTRS
  , trsToMatrix
  ) where

import Control.Lens
import Linear

data TRS a = TRS { trsTranslation :: V3 a
                 , trsRotation :: Quaternion a
                 , trsScale :: V3 a
                 }
           deriving (Show, Eq)

instance RealFloat a => Semigroup (TRS a) where
  a <> b = TRS { trsTranslation = trsTranslation a + trsTranslation b
               , trsRotation = trsRotation a * trsRotation b
               , trsScale = trsScale a * trsScale b
               }

instance RealFloat a => Monoid (TRS a) where
  mempty = TRS { trsTranslation = 0
               , trsRotation = Quaternion 1 0
               , trsScale = 1
               }

-- https://d3cw3dd2w32x2b.cloudfront.net/wp-content/uploads/2015/01/matrix-to-quat.pdf
matrixToQuaternion :: (Ord a, Floating a) => M33 a -> Quaternion a
matrixToQuaternion (V3 (V3 m00 m01 m02)
                       (V3 m10 m11 m12)
                       (V3 m20 m21 m22)) =
  (0.5 / sqrt rt) *^ rq
  where (rt, rq)
          | m22 < 0 =
            if m00 > m11 then
              let t = 1 + m00 - m11 - m22
              in (t, Quaternion t (V3 (m01 + m10) (m20 + m02) (m12 - m21)))
            else
              let t = 1 - m00 + m11 - m22
              in (t, Quaternion (m01 + m10) (V3 t (m12 + m21) (m20 - m02)))
          | otherwise =
            if m00 < -m11 then
              let t = 1 - m00 - m11 + m22
              in (t, Quaternion (m20 + m02) (V3 (m12 + m21) t (m01 - m10)))
            else
              let t = 1 + m00 + m11 + m22
              in (t, Quaternion (m12 - m21) (V3 (m20 - m02) (m01 - m10) t))

matrixToTRS :: (Ord a, Floating a) => M44 a -> TRS a
matrixToTRS mtx = TRS {..}
  where trsTranslation = mtx ^. translation
        getScale col = norm $ mtx ^. column col . _xyz
        trsScale = V3 (getScale _x) (getScale _y) (getScale _z)
        rotationMtx = (mtx ^. _m33) / (V3 trsScale trsScale trsScale)
        trsRotation = matrixToQuaternion rotationMtx

-- FIXME: slow, but too lazy to implement efficiently.
trsToMatrix :: Floating a => TRS a -> M44 a
trsToMatrix (TRS {..}) = transRot & _m33 *~ V3 trsScale trsScale trsScale
  where transRot = mkTransformation trsRotation trsTranslation
