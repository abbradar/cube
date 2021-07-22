-- | Specific functor classes for vectors.

module Data.Vector.Functor where

import Data.Functor.Identity
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import Linear

class UnboxFunctor f where
  fmapUnbox :: (VU.Unbox a, VU.Unbox b) => (a -> b) -> f a -> f b

instance UnboxFunctor VU.Vector where
  fmapUnbox = VU.map

instance UnboxFunctor Identity where
  fmapUnbox = fmap

instance UnboxFunctor V1 where
  fmapUnbox = fmap

instance UnboxFunctor V2 where
  fmapUnbox = fmap

instance UnboxFunctor V3 where
  fmapUnbox = fmap

instance UnboxFunctor V4 where
  fmapUnbox = fmap

instance UnboxFunctor Quaternion where
  fmapUnbox = fmap

class StorableFunctor f where
  fmapStorable :: (VS.Storable a, VS.Storable b) => (a -> b) -> f a -> f b

instance StorableFunctor VS.Vector where
  fmapStorable = VS.map

instance StorableFunctor Identity where
  fmapStorable = fmap

instance StorableFunctor V1 where
  fmapStorable = fmap

instance StorableFunctor V2 where
  fmapStorable = fmap

instance StorableFunctor V3 where
  fmapStorable = fmap

instance StorableFunctor V4 where
  fmapStorable = fmap

instance StorableFunctor Quaternion where
  fmapStorable = fmap
