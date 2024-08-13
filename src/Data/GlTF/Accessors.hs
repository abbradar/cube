-- | Read accessors data into Haskell.

{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.GlTF.Accessors
  ( AccessorContainer(..)
  , AccessorComponent(..)
  , StorableFunctor(..)
  , ConvertedAccessor(..)
  , AccessorMinMax(..)
  , AccessorContainerVector
  , AccessorContainerValue
  , ConvertedAccessorContainer
  , AccessorMinMaxContainer
  , AccessorVectorComponent
  , AccessorValueComponent
  , ConvertedAccessorComponent
  , AccessorMinMaxComponent
  , readRawAccessor
  , prepareAccessor
  , readAccessorMinMax
  ) where

import Control.Monad
import Data.Maybe
import Data.Functor.Identity
import Data.Word
import Data.Int
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector.Storable.ByteString
import Linear hiding (trace)

import Data.Coerce.Functor
import Data.Vector.Functor
import Linear.Aeson
import Linear.Matrix.Wrapper
import Data.GlTF.Types
import Data.GlTF.Resources

-- kek
import Unsafe.Coerce

data AccessorContainer f a = ARScalar (f a)
                           | ARVec2 (f (V2 a))
                           | ARVec3 (f (V3 a))
                           | ARVec4 (f (V4 a))
                           | ARMat2 (f (M22 a))
                           | ARMat3 (f (M33 a))
                           | ARMat4 (f (M44 a))
                           deriving (Functor)

instance StorableFunctor f => StorableFunctor (AccessorContainer f) where
  fmapStorable f (ARScalar x) = ARScalar $ fmapStorable f x
  fmapStorable f (ARVec2 x) = ARVec2 $ fmapStorable (fmap f) x
  fmapStorable f (ARVec3 x) = ARVec3 $ fmapStorable (fmap f) x
  fmapStorable f (ARVec4 x) = ARVec4 $ fmapStorable (fmap f) x
  fmapStorable f (ARMat2 x) = ARMat2 $ fmapStorable (fmap $ fmap f) x
  fmapStorable f (ARMat3 x) = ARMat3 $ fmapStorable (fmap $ fmap f) x
  fmapStorable f (ARMat4 x) = ARMat4 $ fmapStorable (fmap $ fmap f) x
  {-# INLINE fmapStorable #-}

deriving instance ( Show (f a)
                  , Show (f (V2 a))
                  , Show (f (V3 a))
                  , Show (f (V4 a))
                  , Show (f (M22 a))
                  , Show (f (M33 a))
                  , Show (f (M44 a))
                  ) => Show (AccessorContainer f a)

data AccessorComponent f = ARByte (f Int8)
                         | ARUByte (f Word8)
                         | ARShort (f Int16)
                         | ARUShort (f Word16)
                         | ARUInt (f Word32)
                         | ARFloat (f Float)

deriving instance ( Show (f Int8)
                  , Show (f Word8)
                  , Show (f Int16)
                  , Show (f Word16)
                  , Show (f Word32)
                  , Show (f Float)
                  ) => Show (AccessorComponent f)

mapAccessorComponent :: (forall a. (VS.Storable a, Num a) => f a -> f a) -> AccessorComponent f -> AccessorComponent f
mapAccessorComponent f (ARByte x) = ARByte (f x)
mapAccessorComponent f (ARUByte x) = ARUByte (f x)
mapAccessorComponent f (ARShort x) = ARShort (f x)
mapAccessorComponent f (ARUShort x) = ARUShort (f x)
mapAccessorComponent f (ARUInt x) = ARUInt (f x)
mapAccessorComponent f (ARFloat x) = ARFloat (f x)

type AccessorContainerVector = AccessorContainer VS.Vector
type AccessorContainerValue = AccessorContainer Identity

type AccessorVectorComponent = AccessorComponent (AccessorContainer VS.Vector)
type AccessorValueComponent = AccessorComponent (AccessorContainer Identity)

data ConvertedAccessor a = ConvertedAccessor { convertedVector :: VS.Vector a
                                             , convertedMinMax :: AccessorMinMax a
                                             }
--                         deriving (Show, Eq, Ord, Coercible1)
                           deriving (Show, Eq, Ord)

instance StorableFunctor ConvertedAccessor where
  fmapStorable f (ConvertedAccessor {..}) = ConvertedAccessor { convertedVector = fmapStorable f convertedVector
                                                              , convertedMinMax = fmap f convertedMinMax
                                                              }
  {-# INLINE fmapStorable #-}

type ConvertedAccessorContainer = AccessorContainer ConvertedAccessor
type ConvertedAccessorComponent = AccessorComponent (AccessorContainer ConvertedAccessor)

data AccessorMinMax a = AccessorMinMax { convertedMin :: Maybe a
                                       , convertedMax :: Maybe a
                                       }
                      deriving (Show, Eq, Ord, Functor, Coercible1)

instance StorableFunctor AccessorMinMax where
  fmapStorable = fmap

type AccessorMinMaxContainer = AccessorContainer AccessorMinMax
type AccessorMinMaxComponent = AccessorComponent (AccessorContainer AccessorMinMax)

normalizeSigned :: forall a. (Integral a, Bounded a) => a -> Float
normalizeSigned x = max (fromIntegral x / fromIntegral (maxBound :: a)) (-1)

normalizeUnsigned :: forall a. (Integral a, Bounded a) => a -> Float
normalizeUnsigned x = fromIntegral x / fromIntegral (maxBound :: a)

normalizeAccessorComponent :: StorableFunctor f => AccessorComponent (AccessorContainer f) -> AccessorContainer f Float
normalizeAccessorComponent (ARByte vec) = fmapStorable normalizeSigned vec
normalizeAccessorComponent (ARUByte vec) = fmapStorable normalizeUnsigned vec
normalizeAccessorComponent (ARShort vec) = fmapStorable normalizeSigned vec
normalizeAccessorComponent (ARUShort vec) = fmapStorable normalizeUnsigned vec
normalizeAccessorComponent (ARUInt vec) = fmapStorable normalizeUnsigned vec
normalizeAccessorComponent (ARFloat vec) = vec

transposeAccessorMatrices :: forall f. StorableFunctor f => AccessorComponent (AccessorContainer f) -> AccessorComponent (AccessorContainer f)
transposeAccessorMatrices = mapAccessorComponent maybeTranspose
  where maybeTranspose :: forall a. VS.Storable a => AccessorContainer f a -> AccessorContainer f a
        maybeTranspose (ARScalar x) = ARScalar x
        maybeTranspose (ARVec2 x) = ARVec2 x
        maybeTranspose (ARVec3 x) = ARVec3 x
        maybeTranspose (ARVec4 x) = ARVec4 x
        maybeTranspose (ARMat2 x) = ARMat2 $ fmapStorable transpose x
        maybeTranspose (ARMat3 x) = ARMat3 $ fmapStorable transpose x
        maybeTranspose (ARMat4 x) = ARMat4 $ fmapStorable transpose x

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

newtype Converted g = Converted { unConverted :: forall f a. (ParseScientific a, ParseFromList f, VS.Storable (f a)) => Either String (g (f a)) }

{- -readAccessor' :: forall f. (Coercible1 f, StorableFunctor f) => Converted f -> Accessor -> Either String (AccessorComponent (AccessorContainer f))
readAccessor' converted (Accessor {..}) = do
  let accessorContainer :: forall a. (ParseScientific a, VS.Storable a) => Either String (AccessorContainer f a)
      {-# INLINE accessorContainer #-}
      accessorContainer =
        case accessorType of
          ATScalar -> ARScalar <$> coerce1 <$> (unConverted converted :: Either String (f (V1 a)))
          ATVec2 -> ARVec2 <$> unConverted converted
          ATVec3 -> ARVec3 <$> unConverted converted
          ATVec4 -> ARVec4 <$> unConverted converted
          ATMat2 -> ARMat2 <$> coerce1 <$> (unConverted converted :: Either String (f (WM22 a)))
          ATMat3 -> ARMat3 <$> coerce1 <$> (unConverted converted :: Either String (f (WM33 a)))
          ATMat4 -> ARMat4 <$> coerce1 <$> (unConverted converted :: Either String (f (WM44 a)))

  case accessorComponentType of
    CTByte -> ARByte <$> accessorContainer
    CTUnsignedByte -> ARUByte <$> accessorContainer
    CTShort -> ARShort <$> accessorContainer
    CTUnsignedShort -> ARUShort <$> accessorContainer
    CTUnsignedInt -> ARUInt <$> accessorContainer
    CTFloat -> ARFloat <$> accessorContainer
{-# INLINE readAccessor' #-} -}

--kek

readAccessor' :: forall f. (StorableFunctor f) => Converted f -> Accessor -> Either String (AccessorComponent (AccessorContainer f))
readAccessor' converted (Accessor {..}) = do
  let accessorContainer :: forall a. (ParseScientific a, VS.Storable a) => Either String (AccessorContainer f a)
      {-# INLINE accessorContainer #-}
      accessorContainer =
        case accessorType of
          ATScalar -> ARScalar <$> unsafeCoerce <$> (unConverted converted :: Either String (f (V1 a)))
          ATVec2 -> ARVec2 <$> unConverted converted
          ATVec3 -> ARVec3 <$> unConverted converted
          ATVec4 -> ARVec4 <$> unConverted converted
          ATMat2 -> ARMat2 <$> unsafeCoerce <$> (unConverted converted :: Either String (f (WM22 a)))
          ATMat3 -> ARMat3 <$> unsafeCoerce <$> (unConverted converted :: Either String (f (WM33 a)))
          ATMat4 -> ARMat4 <$> unsafeCoerce <$> (unConverted converted :: Either String (f (WM44 a)))

  case accessorComponentType of
    CTByte -> ARByte <$> accessorContainer
    CTUnsignedByte -> ARUByte <$> accessorContainer
    CTShort -> ARShort <$> accessorContainer
    CTUnsignedShort -> ARUShort <$> accessorContainer
    CTUnsignedInt -> ARUInt <$> accessorContainer
    CTFloat -> ARFloat <$> accessorContainer
{-# INLINE readAccessor' #-}

prepareAccessor :: (StorableFunctor f) => Accessor -> AccessorComponent (AccessorContainer f) -> AccessorComponent (AccessorContainer f)
prepareAccessor (Accessor {..}) container = container''
  where container' =
          if fromMaybe False accessorNormalized then
            ARFloat $ normalizeAccessorComponent container
          else
            container
        container'' = transposeAccessorMatrices container'

convertMinMax :: (ParseScientific a, ParseFromList f) => Accessor -> Either String (AccessorMinMax (f a))
convertMinMax (Accessor {..}) = do
  let parseValue vec = do
        conv <- mapM (maybe (Left "Invalid type for value") return . parseScientific) $ V.toList vec
        maybe (Left "Invalid array size for value") return $ parseFromList conv
  convertedMin <- mapM parseValue accessorMin
  convertedMax <- mapM parseValue accessorMax
  return $ AccessorMinMax {..}
{-# INLINE convertMinMax #-}

readAccessorMinMax :: Accessor -> Either String AccessorMinMaxComponent
readAccessorMinMax accessor = prepareAccessor accessor <$> readAccessor' (Converted $ convertMinMax accessor) accessor

readRawAccessor :: V.Vector ByteString -> BufferView -> Accessor -> Either String ConvertedAccessorComponent
readRawAccessor boundBuffers bufferView@(BufferView {..}) accessor@(Accessor {..}) = do
  when (isJust accessorSparse) $ Left "Sparse accessors are not supported"
  buffer <- getBufferViewBuffer boundBuffers bufferView
  unless (accessorIsValid bufferView accessor) $ Left "Buffer view is off buffer bounds"

  let clampedBS = packMatrixColumns accessorType compSize accessorCount $ packFromStrides stride sizeOfElement accessorCount $ B.drop offset buffer

      offset = fromMaybe 0 accessorByteOffset
      compSize = componentSize accessorComponentType
      sizeOfElement = accessorElementSize accessorType accessorComponentType
      stride =
        case fromMaybe 0 viewByteStride of
          0 -> sizeOfElement
          val -> val

      converted = Converted $ do
        let convertedVector = byteStringToVector clampedBS
        convertedMinMax <- convertMinMax accessor
        return $ ConvertedAccessor {..}

  readAccessor' converted accessor
