-- | FromJSON instances for Linear.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Aeson where

import Data.Aeson
import Linear

instance FromJSON a => FromJSON (V2 a) where
  parseJSON val = do
    list <- parseJSON val
    case list of
      [a, b] -> return $ V2 a b
      _ -> fail "Invalid array length for V2"

instance FromJSON a => FromJSON (V3 a) where
  parseJSON val = do
    list <- parseJSON val
    case list of
      [a, b, c] -> return $ V3 a b c
      _ -> fail "Invalid array length for V3"

instance FromJSON a => FromJSON (V4 a) where
  parseJSON val = do
    list <- parseJSON val
    case list of
      [a, b, c, d] -> return $ V4 a b c d
      _ -> fail "Invalid array length for V4"

instance FromJSON a => FromJSON (Quaternion a) where
  parseJSON val = do
    list <- parseJSON val
    case list of
      [w, x, y, z] -> return $ Quaternion w (V3 x y z)
      _ -> fail "Invalid array length for Quaternion"
