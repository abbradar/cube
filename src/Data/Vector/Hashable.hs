-- | Hashable instance for Vectors.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Vector.Hashable where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import Data.Hashable

instance Hashable a => Hashable (V.Vector a) where
  hashWithSalt salt a = hashWithSalt salt $ V.toList a
  hash a = hash $ V.toList a

instance (VU.Unbox a, Hashable a) => Hashable (VU.Vector a) where
  hashWithSalt salt a = hashWithSalt salt $ VU.toList a
  hash a = hash $ VU.toList a

instance (VS.Storable a, Hashable a) => Hashable (VS.Vector a) where
  hashWithSalt salt a = hashWithSalt salt $ VS.toList a
  hash a = hash $ VS.toList a
