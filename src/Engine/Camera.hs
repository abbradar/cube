module Engine.Camera where

import Linear
import Data.Default
import Control.Lens

import Engine.Types

import Debug.Trace

critAngleTan :: Float
critAngleTan = 9.0

data Camera = Camera { _eye :: F3
                     , _lookat :: F3
                     --, _rotation :: QF
                     , _fov :: Float
                     , _ratio :: Float
                     , _nplane :: Float
                     , _fplane :: Float
                     } deriving (Show, Eq)

$(makeLenses ''Camera)

instance Default Camera where
  def = Camera { _eye = (V3 2.0 2.0 (-8.0))
               , _lookat = (V3 0.0 0.0 0.0)
               --, _rotation = axisAngle (V3 0 1 0) 0
               , _fov = pi/4.0
               , _ratio = 4.0/3.0
               , _nplane = 0.1
               , _fplane = 1000.0
               }

projectionMatrix :: Camera -> MF44
projectionMatrix (Camera {..}) = transpose $ perspective _fov _ratio _nplane _fplane

viewMatrix :: Camera -> MF44
viewMatrix (Camera {..}) = transpose $ lookAt _eye _lookat (V3 0.0 0.0 1.0)
    

moveEyes :: F3 -> Camera -> Camera
moveEyes vec cam@(Camera {..}) = cam { _eye = (_eye + vec), _lookat = (_lookat + vec) }

-- Expects relative move of the mouse in range [-1; 1]
rotateEyes :: F2 -> Camera -> Camera
rotateEyes rel@(V2 x y) cam@(Camera {..})
  | nearZero rel = cam
  | otherwise = cam { _eye = _lookat + rotation !* radius }
  where upvect = (V3 0.0 0.0 1.0)
        radius = _eye - _lookat
        slope (V3 xv yv zv) = zv**2/(xv**2 + yv**2)
        ynew 
          | (slope radius > critAngleTan) && (y * (radius ^. _z) < 0) = 0
          | otherwise = y
        rotation = fromQuaternion ((axisAngle (V3 0.0 0.0 1.0) x) *
          (axisAngle (upvect `cross` radius) ynew))
