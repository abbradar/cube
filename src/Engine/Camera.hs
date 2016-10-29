module Engine.Camera where

import Linear
import Data.Default
import Control.Lens

import Engine.Types

import Debug.Trace

critAngleTan :: Float
critAngleTan = 5.0

upvector :: V3 Float
upvector = V3 0.0 1.0 0.0

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
  def = Camera { _eye = (V3 10.0 10.0 (-40.0))
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
viewMatrix (Camera {..}) = transpose $ lookAt _eye _lookat upvector
    

moveEyes :: F3 -> Camera -> Camera
moveEyes vec cam@(Camera {..}) = cam { _eye = (_eye + vec), _lookat = (_lookat + vec) }

-- Expects relative move of the mouse in range [-1; 1]
rotateEyes :: F2 -> Camera -> Camera
rotateEyes rel@(V2 x y) cam@(Camera {..})
  | nearZero rel = cam
  | otherwise = cam { _eye = _lookat + rotation !* radius }
  where upvect = upvector
        radius = _eye - _lookat
        slope (V3 xv yv zv) = yv**2/(xv**2 + zv**2)
        ynew 
          | (slope radius > critAngleTan) && (y * (radius ^. _y) < 0) = 0
          | otherwise = y
        rotation = fromQuaternion ((axisAngle upvector (-x)) *
          (axisAngle (upvect `cross` radius) ynew))
