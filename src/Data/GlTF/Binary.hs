-- | Read GLB 2.0 streams.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Binary
  ( GLB(..)
  , Header(..)
  , Chunk(..)
  , supportedVersion
  , glbParser
  , decodeGlb
  , jsonChunkType
  , binChunkType
  , GLBAsset(..)
  , glbToAsset
  ) where

import Prelude hiding (take)
import Data.Word
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Attoparsec.ByteString
import Data.Attoparsec.Binary
import qualified Data.Aeson as JSON

import Data.GlTF.Types (GlTF)

data GLB = GLB { glbHeader :: Header
               , glbChunks :: [Chunk]
               }
         deriving (Show)

data Header = Header { headerVersion :: Word32
                     }
            deriving (Show)

data Chunk = Chunk { chunkType :: Word32
                   , chunkData :: ByteString
                   }
           deriving (Show)

supportedVersion :: Word32
supportedVersion = 2

headerSize :: Int
headerSize = 12

glbMagic :: Word32
glbMagic = 0x46546C67 -- "glTF"

header :: Parser (Word32, Header)
header = do
  _ <- word32le glbMagic
  headerVersion <- word32le supportedVersion
  glbLength <- anyWord32le
  return (glbLength, Header {..})

chunkHeaderSize :: Int
chunkHeaderSize = 8

chunk :: Parser Chunk
chunk = do
  chunkLength <- anyWord32le
  chunkType <- anyWord32le
  chunkData <- take (fromIntegral chunkLength)
  return Chunk {..}

glbParser :: Parser GLB
glbParser = do
  (totalSize, glbHeader) <- header
  let parseOne leftLength chunks
        | leftLength < 0 = fail "Invalid GLB size field"
        | leftLength > 0 = do
          glbChunk <- chunk
          let newLeftLength = leftLength - chunkHeaderSize - (B.length $ chunkData glbChunk)
          parseOne newLeftLength (glbChunk : chunks)
        | otherwise = return chunks
  revChunks <- parseOne (fromIntegral totalSize - headerSize) []
  return GLB { glbHeader = glbHeader, glbChunks = reverse revChunks }

decodeGlb :: ByteString -> Either String GLB
decodeGlb = parseOnly (glbParser <* endOfInput)

jsonChunkType :: Word32
jsonChunkType = 0x4E4F534A -- "JSON"

binChunkType :: Word32
binChunkType = 0x004E4942 -- "BIN\0"

data GLBAsset = GLBAsset { assetGltf :: GlTF
                         , assetBuffer :: Maybe ByteString
                         }
                deriving (Show)

glbToAsset :: GLB -> Either String GLBAsset
glbToAsset (GLB { glbChunks = jsonChunk : chunks })
  | chunkType jsonChunk == jsonChunkType = do
      assetGltf <- JSON.eitherDecodeStrict' $ chunkData jsonChunk
      let assetBuffer =
            case chunks of
              binaryChunk : _ | chunkType binaryChunk == binChunkType -> Just (chunkData binaryChunk)
              _ -> Nothing
      return GLBAsset {..}
glbToAsset _ = Left "No JSON chunk found"
