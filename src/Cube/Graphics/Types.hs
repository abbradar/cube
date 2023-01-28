-- | Common graphics engine types.

module Cube.Graphics.Types where

import Linear

import Cube.Graphics.TRS
import Cube.Graphics.Camera
import Cube.Graphics.Screen

type V3F = V3 Float
type V4F = V4 Float
type M33F = M33 Float
type M44F = M44 Float
type QF = Quaternion Float

type TRSF = TRS Float
type CameraF = FloatingCamera Float
type ScreenF = Screen Float
