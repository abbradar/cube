module Engine.Camera where

import Linear
import Data.Default
import Control.Lens.TH

import Engine.Types

import Debug.Trace

data Camera = Camera { _eye :: F3
                     , _rotation :: QF
                     , _fov :: Float
                     , _ratio :: Float
                     , _nplane :: Float
                     , _fplane :: Float
                     } deriving (Show, Eq)

$(makeLenses ''Camera)

instance Default Camera where
  def = Camera { _eye = (V3 0.0 0.0 (-8))
               , _rotation = axisAngle (V3 0 1 0) 0
               , _fov = pi/4.0
               , _ratio = 4.0/3.0
               , _nplane = 0.1
               , _fplane = 1000.0
               }

projectionMatrix :: Camera -> MF44
projectionMatrix (Camera {..}) = transpose $ perspective _fov _ratio _nplane _fplane

viewMatrix :: Camera -> MF44
viewMatrix (Camera {..}) = transpose $ mkTransformation _rotation _eye

moveEyes :: F3 -> Camera -> Camera
moveEyes vec cam@(Camera {..}) = cam { _eye = _eye + vec }

-- Expects relative move of the mouse in range [-1; 1]
rotateEyes :: F2 -> Camera -> Camera
rotateEyes rel@(V2 x y) cam@(Camera {..})
  | nearZero rel = cam
  | otherwise = cam { _rotation = axisAngle planeN ang * _rotation }
  where n = V3 0 0 _nplane
        v = V3 x y _nplane
        planeN = n `cross` v
        ang = acos (norm n / norm v)
