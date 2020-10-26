-- | Common graphics engine types.

module Cube.Graphics.Types where

import Linear

import Cube.Graphics.TRS
import Cube.Graphics.Camera
import Cube.Graphics.Screen

type VF3 = V3 Float
type MF33 = M33 Float
type MF44 = M44 Float
type QF = Quaternion Float

type TRSF = TRS Float
type CameraF = Camera Float
type ScreenF = Screen Float
