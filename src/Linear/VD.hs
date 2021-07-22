-- | Dynamically sized vector.

module Linear.VD
  ( VD(..)
  , zipWith
  , singleton
  ) where

import Prelude hiding (zipWith)
import qualified Data.Vector.Unboxed as VU

import Data.Vector.Functor

newtype VD a = VD (VU.Vector a)
             deriving (Show, Eq, Ord)

instance UnboxFunctor VD where
  fmapUnbox f (VD vec) = VD $ fmapUnbox f vec
  {-# INLINE fmapUnbox #-}

zipWith :: (VU.Unbox a, VU.Unbox b, VU.Unbox c) => (a -> b -> c) -> VD a -> VD b -> VD c
zipWith f (VD a) (VD b) = VD $ VU.zipWith f a b
{-# INLINE zipWith #-}

singleton :: VU.Unbox a => a -> VD a
singleton = VD . VU.singleton
{-# INLINE singleton #-}

-- Implementations using `singleton` are not conforming.

instance (VU.Unbox a, Num a) => Num (VD a) where
  (+) = zipWith (+)
  {-# INLINE (+) #-}
  (-) = zipWith (-)
  {-# INLINE (-) #-}
  (*) = zipWith (*)
  {-# INLINE (*) #-}
  negate = fmapUnbox negate
  {-# INLINE negate #-}
  abs = fmapUnbox abs
  {-# INLINE abs #-}
  signum = fmapUnbox signum
  {-# INLINE signum #-}
  fromInteger = singleton . fromInteger
  {-# INLINE fromInteger #-}

instance (VU.Unbox a, Fractional a) => Fractional (VD a) where
  (/) = zipWith (/)
  {-# INLINE (/) #-}
  recip = fmapUnbox recip
  {-# INLINE recip #-}
  fromRational = singleton . fromRational
  {-# INLINE fromRational #-}

instance (VU.Unbox a, Floating a) => Floating (VD a) where
    pi = singleton pi
    {-# INLINE pi #-}
    exp = fmapUnbox exp
    {-# INLINE exp #-}
    sqrt = fmapUnbox sqrt
    {-# INLINE sqrt #-}
    log = fmapUnbox log
    {-# INLINE log #-}
    (**) = zipWith (**)
    {-# INLINE (**) #-}
    logBase = zipWith logBase
    {-# INLINE logBase #-}
    sin = fmapUnbox sin
    {-# INLINE sin #-}
    tan = fmapUnbox tan
    {-# INLINE tan #-}
    cos = fmapUnbox cos
    {-# INLINE cos #-}
    asin = fmapUnbox asin
    {-# INLINE asin #-}
    atan = fmapUnbox atan
    {-# INLINE atan #-}
    acos = fmapUnbox acos
    {-# INLINE acos #-}
    sinh = fmapUnbox sinh
    {-# INLINE sinh #-}
    tanh = fmapUnbox tanh
    {-# INLINE tanh #-}
    cosh = fmapUnbox cosh
    {-# INLINE cosh #-}
    asinh = fmapUnbox asinh
    {-# INLINE asinh #-}
    atanh = fmapUnbox atanh
    {-# INLINE atanh #-}
    acosh = fmapUnbox acosh
    {-# INLINE acosh #-}
