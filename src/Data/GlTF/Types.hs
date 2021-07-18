-- | Basic glTF 2.0 types.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Types
  ( Asset(..)
  , GlTF(..)
  , Version(..)
  , versionText
  , supportedVersion
  , assetIsSupported
  , BufferIndex
  , Buffer(..)
  , BufferViewIndex
  , BufferView(..)
  , AttributeSubIndex
  , AttributeType(..)
  , attributeIsIntegral
  , PrimitiveMode(..)
  , Primitive(..)
  , MeshIndex
  , Mesh(..)
  , NodeIndex
  , Node(..)
  , Scene(..)
  , ComponentType(..)
  , componentSize
  , componentIsInteger
  , AccessorType(..)
  , accessorComponentsNumber
  , AccessorIndex
  , Accessor(..)
  , accessorIsValid
  , MagFilter(..)
  , MinFilter(..)
  , WrappingMode(..)
  , TextureIndex
  , Texture(..)
  , SamplerIndex
  , Sampler(..)
  , defaultSampler
  , NormalTextureInfo(..)
  , PBRMetallicRoughness(..)
  , defaultPBRMetallicRoughness
  , TextureInfo(..)
  , OcclusionTextureInfo(..)
  , MaterialIndex
  , Material(..)
  , defaultMaterial
  , ImageIndex
  , Image(..)
  ) where

import Data.Functor
import Data.Maybe
import Control.Applicative
import Data.Char
import GHC.Generics (Generic)
import qualified Data.Scientific as Scientific
import Data.Vector (Vector)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as T
import Data.HashMap.Strict (HashMap)
import Data.Aeson
import qualified Data.Aeson.Types as JSON
import Data.Hashable
import Linear
import qualified Data.Attoparsec.Text as Atto

import Data.Aeson.Utils
import Linear.Aeson ()

{- These are hand-written, instead of generating from JSON Schema. Reason being
   there is no Draft 4-compatible JSON Schema code generator for Haskell.
-}

gltfOptions :: String -> Options
gltfOptions prefix = defaultOptions { omitNothingFields = False
                                    , fieldLabelModifier = removePrefix prefix
                                    }

supportedVersion :: Version
supportedVersion = Version 2 0

type Extension = Text

data Version = Version { major :: Int
                       , minor :: Int
                       }
             deriving (Show, Eq, Ord, Generic)

versionText :: Version -> String
versionText ver = show (major ver) <> "." <> show (minor ver)

instance FromJSON Version where
  parseJSON = withText "Version" $ \s -> do
    let [Right (major, ""), Right (minor, "")] = T.decimal <$> T.splitOn "." s
    return Version {..}

instance ToJSON Version where
  toJSON = versionText >> toJSON
  toEncoding = versionText >> toEncoding

data Asset = Asset { assetVersion :: Version
                   , assetGenerator :: Maybe Text
                   , assetCopyright :: Maybe Text
                   , assetMinVersion :: Maybe Version
                   , assetExtras :: Maybe (HashMap Text Value)
                   }
           deriving (Show, Generic)

instance FromJSON Asset where
  parseJSON = genericParseJSON $ gltfOptions "asset"

assetIsSupported :: Version -> Asset -> Bool
assetIsSupported myVersion (Asset { assetMinVersion = Just version }) = myVersion >= version
assetIsSupported (Version myMajor _) (Asset { assetVersion = Version theirMajor _ }) = myMajor == theirMajor

data GlTF = GlTF { gltfAsset :: Asset
                 , gltfExtensionsUsed :: Maybe (Set Extension)
                 , gltfExtensionsRequired :: Maybe (Set Extension)
                 , gltfBuffers :: Maybe (Vector Buffer)
                 , gltfBufferViews :: Maybe (Vector BufferView)
                 , gltfAccessors :: Maybe (Vector Accessor)
                 , gltfNodes :: Maybe (Vector Node)
                 , gltfMeshes :: Maybe (Vector Mesh)
                 , gltfScenes :: Maybe (Vector Scene)
                 , gltfSamplers :: Maybe (Vector Sampler)
                 , gltfTextures :: Maybe (Vector Texture)
                 , gltfImages :: Maybe (Vector Image)
                 , gltfMaterials :: Maybe (Vector Material)
                 , gltfExtras :: Maybe (HashMap Text Value)
                 }
          deriving (Show, Generic)

instance FromJSON GlTF where
  parseJSON value = genericParseJSON (gltfOptions "gltf") value

type BufferIndex = Int

data Buffer = Buffer { bufferName :: Maybe Text
                     , bufferUri :: Maybe Text
                     , bufferByteLength :: Int
                     , bufferExtras :: Maybe (HashMap Text Value)
                     }
              deriving (Show, Generic)

instance FromJSON Buffer where
  parseJSON = genericParseJSON $ gltfOptions "buffer"

type BufferViewIndex = Int

data BufferView = BufferView { viewName :: Maybe Text
                             , viewBuffer :: BufferIndex
                             , viewByteOffset :: Maybe Int
                             , viewByteLength :: Int
                             , viewByteStride :: Maybe Int
                             , viewExtras :: Maybe (HashMap Text Value)
                             }
                deriving (Show, Generic)

instance FromJSON BufferView where
  parseJSON = genericParseJSON $ gltfOptions "view"

type AttributeSubIndex = Int

data AttributeType = Position | Normal | Tangent | TexCoord AttributeSubIndex | Color AttributeSubIndex | Joints AttributeSubIndex | Weights AttributeSubIndex
                   deriving (Show, Eq, Ord, Generic, Hashable)

attributeIsIntegral :: AttributeType -> Bool
attributeIsIntegral Position = False
attributeIsIntegral Normal = False
attributeIsIntegral Tangent = False
attributeIsIntegral (TexCoord _) = False
attributeIsIntegral (Color _) = False
attributeIsIntegral (Joints _) = True
attributeIsIntegral (Weights _) = False

prefixedAttribute :: Text -> (Int -> AttributeType) -> Atto.Parser AttributeType
prefixedAttribute prefix constr = do
  _ <- Atto.string prefix
  _ <- Atto.char '_'
  i <- Atto.decimal
  return $ constr i

attributeTypeParser :: Atto.Parser AttributeType
attributeTypeParser =
      (Atto.string "POSITION" $> Position)
  <|> (Atto.string "NORMAL" $> Normal)
  <|> (Atto.string "TANGENT" $> Tangent)
  <|> prefixedAttribute "TEXCOORD" TexCoord
  <|> prefixedAttribute "COLOR" Color
  <|> prefixedAttribute "JOINTS" Joints
  <|> prefixedAttribute "WEIGHTS" Weights

parseAttributeType :: Text -> JSON.Parser AttributeType
parseAttributeType name =
  case Atto.parseOnly (attributeTypeParser <* Atto.endOfInput) name of
    Left e -> fail e
    Right r -> return r

instance FromJSON AttributeType where
  parseJSON = withText "AttributeType" parseAttributeType

instance FromJSONKey AttributeType where
  fromJSONKey = FromJSONKeyTextParser parseAttributeType

data PrimitiveMode = Points | Lines | LineLoop | LineStrip | Triangles | TriangleStrip | TriangleFan
                   deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON PrimitiveMode where
  parseJSON = withScientific "PrimitiveMode" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        0 -> return Points
        1 -> return Lines
        2 -> return LineLoop
        3 -> return LineStrip
        4 -> return Triangles
        5 -> return TriangleStrip
        6 -> return TriangleFan
        _ -> fail $ "Invalid PrimitiveMode value: " ++ show typ
    int -> fail $ "Invalid PrimitiveMode value: " ++ show int

data Primitive = Primitive { primitiveAttributes :: HashMap AttributeType AccessorIndex
                           , primitiveIndices :: Maybe AccessorIndex
                           , primitiveMaterial :: Maybe MaterialIndex
                           , primitiveMode :: Maybe PrimitiveMode
                           , primitiveExtras :: Maybe (HashMap Text Value)
                           }
               deriving (Show, Generic)

instance FromJSON Primitive where
  parseJSON = genericParseJSON $ gltfOptions "primitive"

type MeshIndex = Int

data Mesh = Mesh { meshName :: Maybe Text
                 , meshPrimitives :: Vector Primitive
                 , meshExtras :: Maybe (HashMap Text Value)
                 }
          deriving (Show, Generic)

instance FromJSON Mesh where
  parseJSON = genericParseJSON $ gltfOptions "mesh"

type NodeIndex = Int

data Node = Node { nodeName :: Maybe Text
                 , nodeMesh :: Maybe MeshIndex
                 , nodeRotation :: Maybe (Quaternion Float)
                 , nodeScale :: Maybe (V3 Float)
                 , nodeTranslation :: Maybe (V3 Float)
                 , nodeMatrix :: Maybe (M44 Float)
                 , nodeChildren :: Maybe (Vector NodeIndex)
                 , nodeExtras :: Maybe (HashMap Text Value)
                 }
          deriving (Show, Generic)

instance FromJSON Node where
  parseJSON = withObject "Node" $ \v -> do
    nodeName <- v .:! "name"
    nodeMesh <- v .:! "mesh"

    let parseRotation [x, y, z, w] = return $ Quaternion w (V3 x y z)
        parseRotation _ = fail "Invalid quaternion size"
    nodeRotation <- v .:! "rotation" >>= mapM parseRotation

    nodeScale <- v .:! "scale"
    nodeTranslation <- v .:! "translation"

    let parseMatrix [ m00, m10, m20, m30
                    , m01, m11, m21, m31
                    , m02, m12, m22, m32
                    , m03, m13, m23, m33
                    ] =
          return $ V4 (V4 m00 m01 m02 m03)
                      (V4 m10 m11 m12 m13)
                      (V4 m20 m21 m22 m23)
                      (V4 m30 m31 m32 m33)
        parseMatrix _ = fail "Invalid size matrix"
    nodeMatrix <- v .:! "matrix" >>= mapM parseMatrix

    nodeChildren <- v .:! "children"
    nodeExtras <- v .:! "extras"

    return Node {..}

data Scene = Scene { sceneName :: Maybe Text
                   , sceneNodes :: Set NodeIndex
                   , sceneExtras :: Maybe (HashMap Text Value)
                   }
           deriving (Show, Generic)

instance FromJSON Scene where
  parseJSON = genericParseJSON $ gltfOptions "scene"

data ComponentType = Byte | UnsignedByte | Short | UnsignedShort | UnsignedInt | Float
                   deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON ComponentType where
  parseJSON = withScientific "ComponentType" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        5120 -> return Byte
        5121 -> return UnsignedByte
        5122 -> return Short
        5123 -> return UnsignedShort
        5125 -> return UnsignedInt
        5126 -> return Float
        _ -> fail $ "Invalid ComponentType value: " ++ show typ
    int -> fail $ "Invalid ComponentType value: " ++ show int

componentSize :: ComponentType -> Int
componentSize Byte = 1
componentSize UnsignedByte = 1
componentSize Short = 2
componentSize UnsignedShort = 2
componentSize UnsignedInt = 4
componentSize Float = 4

componentIsInteger :: ComponentType -> Bool
componentIsInteger Byte = True
componentIsInteger UnsignedByte = True
componentIsInteger Short = True
componentIsInteger UnsignedShort = True
componentIsInteger UnsignedInt = True
componentIsInteger Float = False

data AccessorType = Scalar | Vec2 | Vec3 | Vec4 | Mat2 | Mat3 | Mat4
                  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

accessorComponentsNumber :: AccessorType -> Int
accessorComponentsNumber Scalar = 1
accessorComponentsNumber Vec2 = 2
accessorComponentsNumber Vec3 = 3
accessorComponentsNumber Vec4 = 4
accessorComponentsNumber Mat2 = 2 * 2
accessorComponentsNumber Mat3 = 3 * 3
accessorComponentsNumber Mat4 = 4 * 4

instance FromJSON AccessorType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper
                                                }

type AccessorIndex = Int

data Accessor = Accessor { accessorName :: Maybe Text
                         , accessorBufferView :: Maybe BufferViewIndex
                         , accessorByteOffset :: Maybe Int
                         , accessorComponentType :: ComponentType
                         , accessorNormalized :: Maybe Bool
                         , accessorCount :: Int
                         , accessorType :: AccessorType
                         , accessorMax :: Maybe (Vector Float)
                         , accessorMin :: Maybe (Vector Float)
                         , accessorSparse :: Maybe () -- So that we can detect and fail on sparse accessors.
                         , accessorExtras :: Maybe (HashMap Text Value)
                         }
              deriving (Show, Generic)


floatOrNormalized :: Accessor -> Bool
floatOrNormalized (Accessor { accessorComponentType = Float }) = True
floatOrNormalized (Accessor { accessorComponentType = UnsignedByte, accessorNormalized = Just True }) = True
floatOrNormalized (Accessor { accessorComponentType = UnsignedShort, accessorNormalized = Just True }) = True
floatOrNormalized _ = False

accessorIsValid :: AttributeType -> Accessor -> Bool
accessorIsValid Position (Accessor { accessorType = Vec3, accessorComponentType = Float }) = True
accessorIsValid Normal (Accessor { accessorType = Vec3, accessorComponentType = Float }) = True
accessorIsValid Tangent (Accessor { accessorType = Vec4, accessorComponentType = Float }) = True
accessorIsValid (TexCoord _) accessor@(Accessor { accessorType = Vec2 }) = floatOrNormalized accessor
accessorIsValid (Color _) accessor@(Accessor { accessorType = Vec3 }) = floatOrNormalized accessor
accessorIsValid (Color _) accessor@(Accessor { accessorType = Vec4 }) = floatOrNormalized accessor
accessorIsValid (Joints _) (Accessor { accessorType = Vec4, accessorComponentType = UnsignedByte, accessorNormalized = (fromMaybe False -> False) }) = True
accessorIsValid (Joints _) (Accessor { accessorType = Vec4, accessorComponentType = UnsignedShort, accessorNormalized = (fromMaybe False -> False) }) = True
accessorIsValid (Weights _) accessor@(Accessor { accessorType = Vec4 }) = floatOrNormalized accessor
accessorIsValid _ _ = False

instance FromJSON Accessor where
  parseJSON = genericParseJSON $ gltfOptions "accessor"

type TextureIndex = Int

data Texture = Texture { textureName :: Maybe Text
                       , textureSampler :: Maybe SamplerIndex
                       , textureSource :: Maybe ImageIndex
                       , textureExtras :: Maybe (HashMap Text Value)
                       }
             deriving (Show, Generic)

instance FromJSON Texture where
  parseJSON = genericParseJSON $ gltfOptions "texture"

data MagFilter = MagNearest | MagLinear
               deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON MagFilter where
  parseJSON = withScientific "MagFilter" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        9728 -> return MagNearest
        9729 -> return MagLinear
        _ -> fail $ "Invalid MagFilter value: " ++ show typ
    int -> fail $ "Invalid MagFilter value: " ++ show int

data MinFilter = MinNearest | MinLinear | MinNearestMipmapNearest | MinLinearMipmapNearest | MinNearestMipmapLinear | MinLinearMipmapLinear
               deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON MinFilter where
  parseJSON = withScientific "MinFilter" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        9728 -> return MinNearest
        9729 -> return MinLinear
        9984 -> return MinNearestMipmapNearest
        9985 -> return MinLinearMipmapNearest
        9986 -> return MinNearestMipmapLinear
        9987 -> return MinLinearMipmapLinear
        _ -> fail $ "Invalid MinFilter value: " ++ show typ
    int -> fail $ "Invalid MinFilter value: " ++ show int

data WrappingMode = ClampToEdge | MirroredRepeat | Repeat
               deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON WrappingMode where
  parseJSON = withScientific "WrappingMode" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        33071 -> return ClampToEdge
        33648 -> return MirroredRepeat
        10497 -> return Repeat
        _ -> fail $ "Invalid WrappingMode value: " ++ show typ
    int -> fail $ "Invalid WrappingMode value: " ++ show int

type SamplerIndex = Int

data Sampler = Sampler { samplerName :: Maybe Text
                       , samplerMagFilter :: Maybe MagFilter
                       , samplerMinFilter :: Maybe MinFilter
                       , samplerWrapS :: Maybe WrappingMode
                       , samplerWrapT :: Maybe WrappingMode
                       , samplerExtras :: Maybe (HashMap Text Value)
                       }
             deriving (Show, Generic)

instance FromJSON Sampler where
  parseJSON = genericParseJSON $ gltfOptions "sampler"

defaultSampler :: Sampler
defaultSampler = Sampler { samplerName = Nothing
                         , samplerMagFilter = Nothing
                         , samplerMinFilter = Nothing
                         , samplerWrapS = Nothing
                         , samplerWrapT = Nothing
                         , samplerExtras = Nothing
                         }

data AlphaMode = Opaque | Mask | Blend
                deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON AlphaMode where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper
                                                }

data PBRMetallicRoughness = PBRMetallicRoughness { pbrBaseColorFactor :: Maybe (V4 Float)
                                                 , pbrBaseColorTexture :: Maybe TextureInfo
                                                 , pbrMetallicFactor :: Maybe Float
                                                 , pbrRoughnessFactor :: Maybe Float
                                                 , pbrMetallicRoughnessTexture :: Maybe TextureInfo
                                                 }
                          deriving (Show, Generic)

instance FromJSON PBRMetallicRoughness where
  parseJSON = genericParseJSON $ gltfOptions "pbr"

defaultPBRMetallicRoughness :: PBRMetallicRoughness
defaultPBRMetallicRoughness = PBRMetallicRoughness { pbrBaseColorFactor = Nothing
                                                   , pbrBaseColorTexture = Nothing
                                                   , pbrMetallicFactor = Nothing
                                                   , pbrRoughnessFactor = Nothing
                                                   , pbrMetallicRoughnessTexture = Nothing
                                                   }

data OcclusionTextureInfo = OcclusionTextureInfo { occlusionIndex :: TextureIndex
                                                 , occlusionTexCoord :: Maybe Int
                                                 , occlusionStrength :: Maybe Float
                                                 , occlusionExtras :: Maybe (HashMap Text Value)
                                                 }
                          deriving (Show, Generic)

instance FromJSON OcclusionTextureInfo where
  parseJSON = genericParseJSON $ gltfOptions "occlusion"

data TextureInfo = TextureInfo { textureInfoIndex :: TextureIndex
                               , textureInfoTexCoord :: Maybe Int
                               , textureInfoExtras :: Maybe (HashMap Text Value)
                               }
                 deriving (Show, Generic)

instance FromJSON TextureInfo where
  parseJSON = genericParseJSON $ gltfOptions "textureInfo"

type MaterialIndex = Int

data Material = Material { materialName :: Maybe Text
                         , materialPbrMetallicRoughness :: Maybe PBRMetallicRoughness
                         , materialNormalTexture :: Maybe NormalTextureInfo
                         , materialOcclusionTexture :: Maybe OcclusionTextureInfo
                         , materialEmissiveTexture :: Maybe TextureInfo
                         , materialEmissiveFactor :: Maybe (V3 Float)
                         , materialAlphaMode :: Maybe AlphaMode
                         , materialAlphaCutoff :: Maybe Float
                         , materialDoubleSided :: Maybe Bool
                         , materialExtras :: Maybe (HashMap Text Value)
                         }
              deriving (Show, Generic)

defaultMaterial :: Material
defaultMaterial = Material { materialName = Nothing
                           , materialPbrMetallicRoughness = Nothing
                           , materialNormalTexture = Nothing
                           , materialOcclusionTexture = Nothing
                           , materialEmissiveTexture = Nothing
                           , materialEmissiveFactor = Nothing
                           , materialAlphaMode = Nothing
                           , materialAlphaCutoff = Nothing
                           , materialDoubleSided = Nothing
                           , materialExtras = Nothing
                           }

instance FromJSON Material where
  parseJSON = genericParseJSON $ gltfOptions "material"

data NormalTextureInfo = NormalTextureInfo { normalTextureIndex :: TextureIndex
                                           , normalTextureTexCoord :: Maybe Int
                                           , normalTextureScale :: Maybe Float
                                           , normalTextureExtras :: Maybe (HashMap Text Value)
                                           }
                       deriving (Show, Generic)

instance FromJSON NormalTextureInfo where
  parseJSON = genericParseJSON $ gltfOptions "normalTexture"

type ImageIndex = Int

data Image = Image { imageName :: Maybe Text
                   , imageUri :: Maybe Text
                   , imageMimeType :: Maybe Text
                   , imageBufferView :: Maybe BufferViewIndex
                   , imageExtras :: Maybe (HashMap Text Value)
                   }
           deriving (Show, Generic)

instance FromJSON Image where
  parseJSON = genericParseJSON $ gltfOptions "image"
