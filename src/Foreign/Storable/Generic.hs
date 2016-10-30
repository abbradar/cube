-- Most of the code taken from https://github.com/maurer/c-storable-deriving
module Foreign.Storable.Generic
       ( gAlignment
       , gPeek
       , gPoke
       , gSizeOf
       ) where

import Foreign.Ptr
import Foreign.Storable
import GHC.Generics

gPeek :: (Generic a, GStorable (Rep a)) => Ptr a -> IO a
gPeek p = fmap to $ gcPeek 0 (castPtr p)

gPoke :: (Generic a, GStorable (Rep a)) => Ptr a -> a -> IO ()
gPoke p x = gcPoke 0 (castPtr p) $ from x

gAlignment :: (Generic a, GStorable (Rep a)) => a -> Int
gAlignment = gcAlignment . from

gSizeOf :: (Generic a, GStorable (Rep a)) => a -> Int
gSizeOf = gcSizeOf 0 . from

-- | A wrapper class for the raw autoderivation functions,
--   representing what is necessary for the defaulted
--   `PStorable' methods.
--
-- No @(':+:')@ instance.
class GStorable a where
  gcAlignment :: a x -> Int
  gcPeek :: Int -> Ptr (a x)-> IO (a x)
  gcPoke :: Int -> Ptr (a x) -> a x -> IO ()
  gcSizeOf :: Int -> a x -> Int

  -- padding before the field to align from the given offset
  gpPadding :: Int -> a x -> Int
  gpPadding off a = (gcAlignment a - off) `mod` gcAlignment a

instance GStorable U1 where
  gcAlignment _ = 0
  gcPeek _ _ = return U1
  gcPoke _ _ _ = return ()
  gcSizeOf _ _ = 0
  gpPadding _ _ = 0

-- | Test
instance (GStorable a, GStorable b) => GStorable (a :*: b) where
  gcAlignment _ = lcm (gcAlignment (undefined :: a x))
                      (gcAlignment (undefined :: b y))

  gcPeek off p = do
    a <- gcPeek off $ castPtr p
    b <- gcPeek (off + gcSizeOf off a) $ castPtr p
    return $ a :*: b

  gcPoke off p (a :*: b) = do
    gcPoke off (castPtr p) a
    gcPoke (off + gcSizeOf off a) (castPtr p) b

  gcSizeOf off _ =
    let
      a = undefined :: a x
      b = undefined :: b y
      off2 = off + gcSizeOf off a
    in gcSizeOf off a + gcSizeOf off2 b

instance (GStorable a) => GStorable (M1 i c a) where
  gcAlignment (M1 x) = gcAlignment x
  gcPeek off p = M1 <$> gcPeek off (castPtr p)
  gcPoke off p (M1 x) = gcPoke off (castPtr p) x
  gcSizeOf off (M1 x) = gcSizeOf off x
  gpPadding off (M1 x) = gpPadding off x

instance (Storable a) => GStorable (K1 i a) where
  gcAlignment (K1 x) = alignment x
  gcPeek off p = K1 <$> peek (castPtr p `plusPtr` (off + gpPadding off (undefined :: K1 i a x)))
  gcPoke off p (K1 x) = poke (castPtr p `plusPtr` (off + gpPadding off (undefined :: K1 i a x))) x
  gcSizeOf off (K1 x) = gpPadding off (undefined :: K1 i a x) + sizeOf x
