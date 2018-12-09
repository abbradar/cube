module Engine.Camera where

import Linear
import Data.Default
import Control.Lens

import Engine.Types

import Debug.Trace

critAngle :: Float
critAngle = pi/2.1

upvector :: V3 Float
upvector = V3 0.0 0.0 1.0

data Camera = Camera { _eye :: F3
                     , _angles :: F2
                     --, _rotation :: QF
                     , _fov :: Float
                     , _ratio :: Float
                     , _nplane :: Float
                     , _fplane :: Float
                     } deriving (Show, Eq)

$(makeLenses ''Camera)

instance Default Camera where
  def = Camera { _eye = (V3 (0.0) (0.0) (2.0))
               , _angles = (V2 0.0 0.0)
               --, _rotation = axisAngle (V3 0 1 0) 0
               , _fov = pi/4.0
               , _ratio = 4.0/3.0
               , _nplane = 0.1
               , _fplane = 1000.0
               }

projectionMatrix :: Camera -> MF44
projectionMatrix (Camera {..}) = transpose $ perspective _fov _ratio _nplane _fplane

viewMatrix :: Camera -> MF44
viewMatrix (Camera {..}) = let (V2 phi psi) = _angles in transpose $ lookAt _eye (_eye + (V3 (cos(psi)*cos(phi)) (cos(psi)*sin(phi)) (sin(psi)))) upvector
    

moveEye :: F3 -> Camera -> Camera
moveEye vec cam@(Camera {..}) = cam { _eye = (_eye + vec)}

-- Expects relative move of the mouse in range [-1; 1]
rotateEyes :: F2 -> Camera -> Camera
rotateEyes rel@(V2 x y) cam@(Camera {..})
  | nearZero rel = cam
  | otherwise = cam { _angles = normalizeAngles $ _angles + (V2 x y) }
  where normalizeAngles (V2 x' y')
          | y' > critAngle = (V2 (modulo2pi x') critAngle)
          | y' < -critAngle = (V2 (modulo2pi x') (-critAngle))
          | otherwise = (V2 (modulo2pi x') y')
        modulo2pi x'
          | x' > 2*pi = modulo2pi (x' - 2*pi)
          | x' < 0 = modulo2pi (x' + 2*pi)
          | otherwise = x'


  ------------------------------------- getters ------------------------------
getHorizontal :: Camera -> HorizontalPos
getHorizontal (Camera {..}) = let (V3 x y _) = _eye in (V2 (round x) (round y))
