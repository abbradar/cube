module Engine.Camera where

import Linear.Matrix
import Linear.Projection
import Linear.V3
import Data.Default

data Camera = Camera { eye :: V3 Float
                     , center :: V3 Float
                     , up :: V3 Float
                     , fov :: Float
                     , ratio :: Float
                     , nplane :: Float
                     , fplane :: Float
                     } deriving (Show, Eq)

instance Default Camera where
  def = Camera { eye = (V3 5.0 0.0 5.0)
               , center = (V3 0.0 0.0 0.0)
               , up = (V3 0.0 1.0 0.0)
               , fov = pi/4.0
               , ratio = 4.0/3.0
               , nplane = 0.1
               , fplane = 1000.0
               }

projectionMatrix :: Camera -> M44 Float
projectionMatrix (Camera {..}) = transpose $ perspective fov ratio nplane fplane

viewMatrix :: Camera -> M44 Float
viewMatrix (Camera {..}) = transpose $ lookAt eye center up
