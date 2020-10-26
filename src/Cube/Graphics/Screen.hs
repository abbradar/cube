-- | Screen projection (aspect ratio, perspective etc.)

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Screen
  ( Screen(..)
  , defaultNearPlane
  , defaultFarPlane
  , perspectiveScreen
  ) where

import Linear

data Screen a = Screen { projectionMatrix :: M44 a
                       }
              deriving (Show, Eq)

defaultNearPlane :: Floating a => a
defaultNearPlane = 0.1

defaultFarPlane :: Floating a => a
defaultFarPlane = 1000.0

perspectiveScreen :: Floating a => a -> a -> a -> a -> Screen a
perspectiveScreen fov ratio nplane fplane = Screen $ perspective fov ratio nplane fplane
