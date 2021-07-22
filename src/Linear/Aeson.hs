-- | FromJSON instances for Linear.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Linear.Aeson
  ( ParseFromList(..)
  , ParseScientific(..)
  ) where

import Control.Monad
import Data.Scientific
import Data.Word
import Data.Int
import Data.Aeson
import Linear

import Linear.Matrix.Wrapper

class ParseFromList f where
  parseFromList :: [a] -> Maybe (f a)

instance ParseFromList V1 where
  parseFromList [a] = Just $ V1 a
  parseFromList _ = Nothing

instance ParseFromList V2 where
  parseFromList [a, b] = Just $ V2 a b
  parseFromList _ = Nothing

instance ParseFromList V3 where
  parseFromList [a, b, c] = Just $ V3 a b c
  parseFromList _ = Nothing

instance ParseFromList V4 where
  parseFromList [a, b, c, d] = Just $ V4 a b c d
  parseFromList _ = Nothing

instance ParseFromList Quaternion where
  parseFromList [w, x, y, z] = Just $ Quaternion w (V3 x y z)
  parseFromList _ = Nothing

instance ParseFromList WM22 where
  parseFromList [ m00, m01
                , m10, m11
                ] = Just $ WM22 (V2 (V2 m00 m01)
                                    (V2 m10 m11))
  parseFromList _ = Nothing

instance ParseFromList WM33 where
  parseFromList [ m00, m01, m02
                , m10, m11, m12
                , m20, m21, m22
                ] = Just $ WM33 (V3 (V3 m00 m01 m02)
                                    (V3 m10 m11 m12)
                                    (V3 m20 m21 m22))
  parseFromList _ = Nothing

instance ParseFromList WM44 where
  parseFromList [ m00, m01, m02, m03
                , m10, m11, m12, m13
                , m20, m21, m22, m23
                , m30, m31, m32, m33
                ] = Just $ WM44 (V4 (V4 m00 m01 m02 m03)
                                    (V4 m10 m11 m12 m13)
                                    (V4 m20 m21 m22 m23)
                                    (V4 m30 m31 m32 m33))
  parseFromList _ = Nothing

class ParseScientific a where
  parseScientific :: Scientific -> Maybe a

parseBoundedRealFloat :: (RealFloat a) => Scientific -> Maybe a
parseBoundedRealFloat (toBoundedRealFloat -> Right i) = Just i
parseBoundedRealFloat _ = Nothing

instance ParseScientific Word8 where
  parseScientific = toBoundedInteger

instance ParseScientific Int8 where
  parseScientific = toBoundedInteger

instance ParseScientific Word16 where
  parseScientific = toBoundedInteger

instance ParseScientific Int16 where
  parseScientific = toBoundedInteger

instance ParseScientific Word32 where
  parseScientific = toBoundedInteger

instance ParseScientific Int32 where
  parseScientific = toBoundedInteger

instance ParseScientific Word64 where
  parseScientific = toBoundedInteger

instance ParseScientific Int64 where
  parseScientific = toBoundedInteger

instance ParseScientific Float where
  parseScientific = parseBoundedRealFloat

instance ParseScientific Double where
  parseScientific = parseBoundedRealFloat

instance FromJSON a => FromJSON (V2 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for V2") return . parseFromList

instance FromJSON a => FromJSON (V3 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for V3") return . parseFromList

instance FromJSON a => FromJSON (V4 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for V4") return . parseFromList

instance FromJSON a => FromJSON (Quaternion a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for Quaternion") return . parseFromList

instance FromJSON a => FromJSON (WM22 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for WM22") return . parseFromList

instance FromJSON a => FromJSON (WM33 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for WM33") return . parseFromList

instance FromJSON a => FromJSON (WM44 a) where
  parseJSON = parseJSON >=> maybe (fail "Invalid array length for WM44") return . parseFromList
