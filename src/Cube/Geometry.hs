-- | Geometry-related utils.

module Cube.Geometry
  ( matrixToQuaternion
  ) where

import Linear

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
