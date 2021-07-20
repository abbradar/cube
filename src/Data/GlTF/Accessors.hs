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
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.ByteString (ByteString)
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

buildFromStrides :: Int -> Int -> Int -> ByteString -> [ByteString]
buildFromStrides _stride _sizeOfElement 0 _bs = []
buildFromStrides stride sizeOfElement count bs = B.take sizeOfElement bs : buildFromStrides stride sizeOfElement (count - 1) (B.drop stride bs)

packFromStrides :: Int -> Int -> Int -> ByteString -> ByteString
packFromStrides stride sizeOfElement count bs
  | stride == sizeOfElement = B.take packedSize bs
  | otherwise = B.concat $ buildFromStrides stride sizeOfElement count bs
  where packedSize = sizeOfElement * count

-- https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md#data-alignment
buildMatrixColumns :: Int -> Int -> Int -> ByteString -> [ByteString]
buildMatrixColumns _columnSize _alignedColumnSize 0 _bs = []
buildMatrixColumns columnSize alignedColumnSize count bs = B.take columnSize bs : buildMatrixColumns columnSize alignedColumnSize (count - 1) (B.drop alignedColumnSize bs)

packMatrixColumns :: AccessorType -> Int -> Int -> ByteString -> ByteString
packMatrixColumns accessorType compSize count bs
  | rows == 1 || columnSize `mod` 4 == 0 = bs
  | otherwise = B.concat $ buildMatrixColumns columnSize alignedColumnSize (count * rows) bs
  where cols = accessorColumns accessorType
        rows = accessorRows accessorType
        columnSize = compSize * cols
        columnTrail = columnSize `mod` accessorMatrixColumnsAlignment
        alignedColumnSize = columnSize + (accessorMatrixColumnsAlignment - columnTrail)

-- This is valid only for non-sparse accessors!
readRawAccessorWithBuffer :: V.Vector ByteString -> BufferView -> Accessor -> Either String AccessorRawData
readRawAccessorWithBuffer boundBuffers bufferView@(BufferView {..}) accessor@(Accessor {..}) = do
  buffer <- getBufferViewBuffer boundBuffers bufferView
  unless (accessorIsValid bufferView accessor) $ Left "Buffer view is off buffer bounds"
  let clampedBS = packMatrixColumns accessorType compSize accessorCount $ packFromStrides stride sizeOfElement accessorCount $ B.drop offset buffer

      vec :: forall a. VS.Storable a => VS.Vector a
      vec = byteStringToVector clampedBS
      accessorVec :: forall a. VS.Storable a => AccessorRawVector a
      accessorVec =
        case accessorType of
          ATScalar -> ARScalar vec
          ATVec2 -> ARVec2 vec
          ATVec3 -> ARVec3 vec
          ATVec4 -> ARVec4 vec
          ATMat2 -> ARMat2 $ VS.map transpose vec
          ATMat3 -> ARMat3 $ VS.map transpose vec
          ATMat4 -> ARMat4 $ VS.map transpose vec

  return $
    case accessorComponentType of
      CTByte -> ARByte accessorVec
      CTUnsignedByte -> ARUByte accessorVec
      CTShort -> ARShort accessorVec
      CTUnsignedShort -> ARUShort accessorVec
      CTUnsignedInt -> ARUInt accessorVec
      CTFloat -> ARFloat accessorVec

  where offset = fromMaybe 0 accessorByteOffset
        compSize = componentSize accessorComponentType
        sizeOfElement = accessorElementSize accessorType accessorComponentType
        stride =
          case fromMaybe 0 viewByteStride of
            0 -> sizeOfElement
            val -> val
