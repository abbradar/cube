-- | Resolve external resources and load everything into memeory.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Resources
  ( BoundBufferView(..)
  , BoundBufferViews
  , BoundGlTF(..)
  , loadFiles
  ) where

import Control.Applicative
import Control.Monad
import Data.Foldable
import Data.Maybe
import Data.Char
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import System.FilePath
import qualified Data.Aeson as JSON
import System.IO.Posix.MMap
import Codec.Picture (DynamicImage, decodeImage)
import Codec.Picture.Jpg
import Codec.Picture.Png
import Control.Concurrent.Async
import Data.String.Interpolate

import Data.GlTF.Types as GlTF
import Data.GlTF.Binary
import Data.GlTF.URI

data BoundBufferView = BoundBufferView { boundBufferView :: BufferView
                                       , boundBufferRaw :: ByteString
                                       }
                     deriving (Show)

type BoundBufferViews = HashMap BufferViewIndex BoundBufferView

data BoundGlTF = BoundGlTF { boundGltf :: GlTF
                           , boundBufferViews :: Vector BoundBufferView
                           , boundImages :: Vector DynamicImage
                           }

loadUri :: FilePath -> Text -> IO (Maybe TypePair, ByteString)
loadUri basePath uriStr =
  case decodeGltfUri uriStr of
    Left err -> fail [i|Error while decoding URI: #{err}|]
    Right (GlFileURI path) -> do
      file <- unsafeMMapFile (basePath </> path)
      let media =
            case map toLower $ snd $ splitExtension path of
              ".png" -> Just ("image", "png")
              ".jpg" -> Just ("image", "jpeg")
              ".jpeg" -> Just ("image", "jpeg")
              _ -> Nothing
      return (media, file)
    Right (GlDataURI dataUri) -> return (fmap typePair $ uriMediaType dataUri, uriData dataUri)

loadBuffers :: FilePath -> Maybe ByteString -> Vector Buffer -> IO (Vector ByteString)
loadBuffers basePath maybeIntBuffer initBuffers = do
  promises <- reverse <$> snd <$> foldlM doLoad (maybeIntBuffer, []) initBuffers
  V.fromList <$> mapM awaitBuffer promises

  where doLoad (Just intBuffer, ret) buf@(Buffer { bufferUri = Nothing }) =
          return (Nothing, Left bufStr : ret)
          where bufStr = B.take (bufferByteLength buf) intBuffer
        doLoad _ (Buffer { bufferUri = Nothing }) = fail "Invalid buffer with no URI"
        doLoad (_, ret) buf@(Buffer { bufferUri = Just bufferUri }) = do
          bufPromise <- async $ B.take (bufferByteLength buf) <$> snd <$> loadUri basePath bufferUri
          return (Nothing, Right bufPromise : ret)

        awaitBuffer (Left buf) = return buf
        awaitBuffer (Right promise) = wait promise

getBufferView :: Vector ByteString -> BufferView -> Either String BoundBufferView
getBufferView buffers boundBufferView@(BufferView {..}) = do
  when (viewBuffer < 0 || viewBuffer >= V.length buffers) $ Left [i|Buffer index is invalid: #{viewBuffer}|]
  let buffer = buffers V.! viewBuffer
      offset = fromMaybe 0 viewByteOffset
  when (offset < 0) $ Left "Buffer view offset is less than zero"
  when (viewByteLength < 0) $ Left "Buffer view length is less than zero"
  when (offset + viewByteLength > B.length buffer) $ Left "Buffer view is off buffer bounds"
  let boundBufferRaw = B.take viewByteLength $ B.drop offset buffer
  return $ BoundBufferView {..}

loadImage :: FilePath -> Vector BoundBufferView -> Image -> IO DynamicImage
loadImage basePath bufferViews img = do
  let parseMediaType typStr =
        case decodeMediaType typStr of
          Left err -> fail [i|Cannot decode image media type: #{err}|]
          Right t -> return $ typePair t
  imgMedia <- mapM parseMediaType $ imageMimeType img
  (media, file) <-
    case img of
      Image { imageUri = Just uri, imageBufferView = Nothing } -> do
        (uriMedia, file) <- loadUri basePath uri
        return (imgMedia <|> uriMedia, file)
      Image { imageUri = Nothing, imageBufferView = Just idx } -> do
        let file = boundBufferRaw $ bufferViews V.! idx
        return (imgMedia, file)
      _ -> fail "Invalid image"
  let decode =
        case media of
          Just ("image", "png") -> decodePng
          Just ("image", "jpeg") -> decodeJpeg
          _ -> decodeImage
  case decode file of
    Left err -> fail err
    Right rimg -> return rimg

loadFiles :: FilePath -> IO BoundGlTF
loadFiles path = do
  (boundGltf, intBuffer) <-
    case map toLower $ snd $ splitExtension path of
      ".glb" -> do
        file <- unsafeMMapFile path
        rawAsset <-
          case decodeGlb file of
            Left err -> fail [i|Failed to decode GLB: #{err}|]
            Right a -> return a
        asset <-
          case glbToAsset rawAsset of
            Left e -> fail [i|Invalid GLB file layout: #{e}|]
            Right r -> return r
        return (assetGltf asset, assetBuffer asset)
      ".gltf" -> do
        res <- JSON.eitherDecodeFileStrict' path
        case res of
          Left e -> fail e
          Right glTF -> return (glTF, Nothing)
      ext -> fail [i|Unknown file extension: #{ext}|]
  unless (assetIsSupported GlTF.supportedVersion $ gltfAsset boundGltf) $ fail "Unsupported glTF version"
  let basePath = takeDirectory path
  boundBuffers <- loadBuffers basePath intBuffer (fromMaybe V.empty $ gltfBuffers boundGltf)
  boundBufferViews <-
    case mapM (getBufferView boundBuffers) $ fromMaybe V.empty $ gltfBufferViews boundGltf of
      Left e -> fail $ "Failed to bind buffer views: " ++ e
      Right r -> return r
  boundImages <- mapConcurrently (loadImage basePath boundBufferViews) $ fromMaybe V.empty $ gltfImages boundGltf
  return BoundGlTF {..}
