-- | Read accessors data into Haskell.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Accessors
  ( AccessorRawVector(..)
  , AccessorRawData(..)
  , readRawAccessorWithBuffer
  ) where

import Control.Monad
import Data.Maybe
import Data.Word
import Data.Int
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B
import Data.Vector.Storable.ByteString
import Linear

import Data.GlTF.Types
import Data.GlTF.Resources

data AccessorRawVector a = ARScalar (VS.Vector a)
                         | ARVec2 (VS.Vector (V2 a))
                         | ARVec3 (VS.Vector (V3 a))
                         | ARVec4 (VS.Vector (V4 a))
                         | ARMat2 (VS.Vector (M22 a))
                         | ARMat3 (VS.Vector (M33 a))
                         | ARMat4 (VS.Vector (M44 a))
                         deriving (Show, Eq, Ord)

data AccessorRawData = ARByte (AccessorRawVector Int8)
                     | ARUByte (AccessorRawVector Word8)
                     | ARShort (AccessorRawVector Int16)
                     | ARUShort (AccessorRawVector Word16)
                     | ARUInt (AccessorRawVector Word32)
                     | ARFloat (AccessorRawVector Float)
                     deriving (Show, Eq, Ord)

-- This is valid only for non-sparse accessors!
readRawAccessorWithBuffer :: BoundBufferView -> Accessor -> Either String AccessorRawData
readRawAccessorWithBuffer (BoundBufferView {..}) (Accessor {..}) = do
  let offset = fromMaybe 0 accessorByteOffset
      stride = fromMaybe 0 $ viewByteStride boundBufferView
      size = componentSize accessorComponentType * accessorCount
  unless (stride == 0) $ Left "Strides in accessors are not supported"
  when (offset + size > B.length boundBufferRaw) $ Left "Buffer view is off buffer bounds"
  return $
    let vec :: forall a. VS.Storable a => VS.Vector a
        vec = byteStringToVector $ B.take size $ B.drop offset boundBufferRaw
        accessorVec :: forall a. VS.Storable a => AccessorRawVector a
        accessorVec =
          case accessorType of
            Scalar -> ARScalar vec
            Vec2 -> ARVec2 vec
            Vec3 -> ARVec3 vec
            Vec4 -> ARVec4 vec
            Mat2 -> ARMat2 vec
            Mat3 -> ARMat3 vec
            Mat4 -> ARMat4 vec
    in case accessorComponentType of
      Byte -> ARByte accessorVec
      UnsignedByte -> ARUByte accessorVec
      Short -> ARShort accessorVec
      UnsignedShort -> ARUShort accessorVec
      UnsignedInt -> ARUInt accessorVec
      Float -> ARFloat accessorVec
