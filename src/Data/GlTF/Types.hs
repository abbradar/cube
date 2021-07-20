-- | Basic glTF 2.0 types.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Types
  ( Asset(..)
  , GlTF(..)
  , Version(..)
  , ExtrasMap
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
  , accessorMatrixColumnsAlignment
  , accessorComponentsNumber
  , accessorRows
  , accessorColumns
  , accessorElementSize
  , AccessorIndex
  , Accessor(..)
  , accessorIsValid
  , attributeAccessorIsValid
  , MagFilter(..)
  , MinFilter(..)
  , WrappingMode(..)
  , TextureIndex
  , Texture(..)
  , SamplerIndex
  , Sampler(..)
  , defaultSampler
  , NormalTextureInfo(..)
  , AlphaMode(..)
  , PBRMetallicRoughness(..)
  , defaultPBRMetallicRoughness
  , TextureInfo(..)
  , OcclusionTextureInfo(..)
  , MaterialIndex
  , Material(..)
  , defaultMaterial
  , ImageIndex
  , Image(..)
  , Animation(..)
  , Channel(..)
  , Target(..)
  , TargetPath(..)
  , AnimationSamplerInterpolation(..)
  , AnimationSampler(..)
  , AnimationSamplerIndex
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

type ExtrasMap = HashMap Text Value

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
                   , assetExtras :: Maybe ExtrasMap
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
                 , gltfAnimations :: Maybe (Vector Animation)
                 , gltfExtras :: Maybe ExtrasMap
                 }
          deriving (Show, Generic)

instance FromJSON GlTF where
  parseJSON value = genericParseJSON (gltfOptions "gltf") value

type BufferIndex = Int

data Buffer = Buffer { bufferName :: Maybe Text
                     , bufferUri :: Maybe Text
                     , bufferByteLength :: Int
                     , bufferExtras :: Maybe ExtrasMap
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
                             , viewExtras :: Maybe ExtrasMap
                             }
                deriving (Show, Generic)

instance FromJSON BufferView where
  parseJSON = genericParseJSON $ gltfOptions "view"

type AttributeSubIndex = Int

data AttributeType = ATPosition | ATNormal | ATTangent | ATTexCoord AttributeSubIndex | ATColor AttributeSubIndex | ATJoints AttributeSubIndex | ATWeights AttributeSubIndex
                   deriving (Show, Eq, Ord, Generic, Hashable)

attributeIsIntegral :: AttributeType -> Bool
attributeIsIntegral ATPosition = False
attributeIsIntegral ATNormal = False
attributeIsIntegral ATTangent = False
attributeIsIntegral (ATTexCoord _) = False
attributeIsIntegral (ATColor _) = False
attributeIsIntegral (ATJoints _) = True
attributeIsIntegral (ATWeights _) = False

prefixedAttribute :: Text -> (Int -> AttributeType) -> Atto.Parser AttributeType
prefixedAttribute prefix constr = do
  _ <- Atto.string prefix
  _ <- Atto.char '_'
  i <- Atto.decimal
  return $ constr i

attributeTypeParser :: Atto.Parser AttributeType
attributeTypeParser =
      (Atto.string "POSITION" $> ATPosition)
  <|> (Atto.string "NORMAL" $> ATNormal)
  <|> (Atto.string "TANGENT" $> ATTangent)
  <|> prefixedAttribute "TEXCOORD" ATTexCoord
  <|> prefixedAttribute "COLOR" ATColor
  <|> prefixedAttribute "JOINTS" ATJoints
  <|> prefixedAttribute "WEIGHTS" ATWeights

parseAttributeType :: Text -> JSON.Parser AttributeType
parseAttributeType name =
  case Atto.parseOnly (attributeTypeParser <* Atto.endOfInput) name of
    Left e -> fail e
    Right r -> return r

instance FromJSON AttributeType where
  parseJSON = withText "AttributeType" parseAttributeType

instance FromJSONKey AttributeType where
  fromJSONKey = FromJSONKeyTextParser parseAttributeType

data PrimitiveMode = PMPoints | PMLines | PMLineLoop | PMLineStrip | PMTriangles | PMTriangleStrip | PMTriangleFan
                   deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON PrimitiveMode where
  parseJSON = withScientific "PrimitiveMode" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        0 -> return PMPoints
        1 -> return PMLines
        2 -> return PMLineLoop
        3 -> return PMLineStrip
        4 -> return PMTriangles
        5 -> return PMTriangleStrip
        6 -> return PMTriangleFan
        _ -> fail $ "Invalid PrimitiveMode value: " ++ show typ
    int -> fail $ "Invalid PrimitiveMode value: " ++ show int

data Primitive = Primitive { primitiveAttributes :: HashMap AttributeType AccessorIndex
                           , primitiveIndices :: Maybe AccessorIndex
                           , primitiveMaterial :: Maybe MaterialIndex
                           , primitiveMode :: Maybe PrimitiveMode
                           , primitiveExtras :: Maybe ExtrasMap
                           }
               deriving (Show, Generic)

instance FromJSON Primitive where
  parseJSON = genericParseJSON $ gltfOptions "primitive"

type MeshIndex = Int

data Mesh = Mesh { meshName :: Maybe Text
                 , meshPrimitives :: Vector Primitive
                 , meshExtras :: Maybe ExtrasMap
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
                 , nodeExtras :: Maybe ExtrasMap
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
                   , sceneExtras :: Maybe ExtrasMap
                   }
           deriving (Show, Generic)

instance FromJSON Scene where
  parseJSON = genericParseJSON $ gltfOptions "scene"

data ComponentType = CTByte | CTUnsignedByte | CTShort | CTUnsignedShort | CTUnsignedInt | CTFloat
                   deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON ComponentType where
  parseJSON = withScientific "ComponentType" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        5120 -> return CTByte
        5121 -> return CTUnsignedByte
        5122 -> return CTShort
        5123 -> return CTUnsignedShort
        5125 -> return CTUnsignedInt
        5126 -> return CTFloat
        _ -> fail $ "Invalid ComponentType value: " ++ show typ
    int -> fail $ "Invalid ComponentType value: " ++ show int

componentSize :: ComponentType -> Int
componentSize CTByte = 1
componentSize CTUnsignedByte = 1
componentSize CTShort = 2
componentSize CTUnsignedShort = 2
componentSize CTUnsignedInt = 4
componentSize CTFloat = 4

componentIsInteger :: ComponentType -> Bool
componentIsInteger CTByte = True
componentIsInteger CTUnsignedByte = True
componentIsInteger CTShort = True
componentIsInteger CTUnsignedShort = True
componentIsInteger CTUnsignedInt = True
componentIsInteger CTFloat = False

data AccessorType = ATScalar | ATVec2 | ATVec3 | ATVec4 | ATMat2 | ATMat3 | ATMat4
                  deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

accessorComponentsNumber :: AccessorType -> Int
accessorComponentsNumber typ = accessorRows typ * accessorColumns typ

-- https://github.com/KhronosGroup/glTF/blob/master/specification/2.0/README.md#data-alignment
accessorMatrixColumnsAlignment :: Int
accessorMatrixColumnsAlignment = 4

accessorRows :: AccessorType -> Int
accessorRows ATScalar = 1
accessorRows ATVec2 = 1
accessorRows ATVec3 = 1
accessorRows ATVec4 = 1
accessorRows ATMat2 = 2
accessorRows ATMat3 = 3
accessorRows ATMat4 = 4

accessorColumns :: AccessorType -> Int
accessorColumns ATScalar = 1
accessorColumns ATVec2 = 2
accessorColumns ATVec3 = 3
accessorColumns ATVec4 = 4
accessorColumns ATMat2 = 2
accessorColumns ATMat3 = 3
accessorColumns ATMat4 = 4

accessorElementSize :: AccessorType -> ComponentType -> Int
accessorElementSize accessorType componentType
  | rows == 1 = columnSize
  | otherwise = matrixColumnSize * rows
  where cols = accessorColumns accessorType
        rows = accessorRows accessorType
        compSize = componentSize componentType
        columnSize = compSize * cols
        matrixColumnTrail = columnSize `mod` accessorMatrixColumnsAlignment
        matrixColumnSize
          | matrixColumnTrail == 0 = columnSize
          | otherwise = columnSize + (accessorMatrixColumnsAlignment - matrixColumnTrail)

accessorIsValid :: BufferView -> Accessor -> Bool
accessorIsValid (BufferView {..}) (Accessor {..}) = offset + stride * (accessorCount - 1) + sizeOfElement <= viewByteLength
  where offset = fromMaybe 0 accessorByteOffset
        sizeOfElement = accessorElementSize accessorType accessorComponentType
        stride =
          case fromMaybe 0 viewByteStride of
            0 -> sizeOfElement
            val -> val

instance FromJSON AccessorType where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper . removePrefix "AT"
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
                         , accessorExtras :: Maybe ExtrasMap
                         }
              deriving (Show, Generic)


floatOrNormalized :: Accessor -> Bool
floatOrNormalized (Accessor { accessorComponentType = CTFloat }) = True
floatOrNormalized (Accessor { accessorComponentType = CTUnsignedByte, accessorNormalized = Just True }) = True
floatOrNormalized (Accessor { accessorComponentType = CTUnsignedShort, accessorNormalized = Just True }) = True
floatOrNormalized _ = False

attributeAccessorIsValid :: AttributeType -> Accessor -> Bool
attributeAccessorIsValid ATPosition (Accessor { accessorType = ATVec3, accessorComponentType = CTFloat }) = True
attributeAccessorIsValid ATNormal (Accessor { accessorType = ATVec3, accessorComponentType = CTFloat }) = True
attributeAccessorIsValid ATTangent (Accessor { accessorType = ATVec4, accessorComponentType = CTFloat }) = True
attributeAccessorIsValid (ATTexCoord _) accessor@(Accessor { accessorType = ATVec2 }) = floatOrNormalized accessor
attributeAccessorIsValid (ATColor _) accessor@(Accessor { accessorType = ATVec3 }) = floatOrNormalized accessor
attributeAccessorIsValid (ATColor _) accessor@(Accessor { accessorType = ATVec4 }) = floatOrNormalized accessor
attributeAccessorIsValid (ATJoints _) (Accessor { accessorType = ATVec4, accessorComponentType = CTUnsignedByte, accessorNormalized = (fromMaybe False -> False) }) = True
attributeAccessorIsValid (ATJoints _) (Accessor { accessorType = ATVec4, accessorComponentType = CTUnsignedShort, accessorNormalized = (fromMaybe False -> False) }) = True
attributeAccessorIsValid (ATWeights _) accessor@(Accessor { accessorType = ATVec4 }) = floatOrNormalized accessor
attributeAccessorIsValid _ _ = False

instance FromJSON Accessor where
  parseJSON = genericParseJSON $ gltfOptions "accessor"

type TextureIndex = Int

data Texture = Texture { textureName :: Maybe Text
                       , textureSampler :: Maybe SamplerIndex
                       , textureSource :: Maybe ImageIndex
                       , textureExtras :: Maybe ExtrasMap
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

data WrappingMode = WMClampToEdge | WMMirroredRepeat | WMRepeat
               deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON WrappingMode where
  parseJSON = withScientific "WrappingMode" $ \case
    (Scientific.toBoundedInteger -> Just (typ :: Int)) ->
      case typ of
        33071 -> return WMClampToEdge
        33648 -> return WMMirroredRepeat
        10497 -> return WMRepeat
        _ -> fail $ "Invalid WrappingMode value: " ++ show typ
    int -> fail $ "Invalid WrappingMode value: " ++ show int

type SamplerIndex = Int

data Sampler = Sampler { samplerName :: Maybe Text
                       , samplerMagFilter :: Maybe MagFilter
                       , samplerMinFilter :: Maybe MinFilter
                       , samplerWrapS :: Maybe WrappingMode
                       , samplerWrapT :: Maybe WrappingMode
                       , samplerExtras :: Maybe ExtrasMap
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

data AlphaMode = AMOpaque | AMMask | AMBlend
                deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON AlphaMode where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper . removePrefix "AM"
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
                                                 , occlusionExtras :: Maybe ExtrasMap
                                                 }
                          deriving (Show, Generic)

instance FromJSON OcclusionTextureInfo where
  parseJSON = genericParseJSON $ gltfOptions "occlusion"

data TextureInfo = TextureInfo { textureInfoIndex :: TextureIndex
                               , textureInfoTexCoord :: Maybe Int
                               , textureInfoExtras :: Maybe ExtrasMap
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
                         , materialExtras :: Maybe ExtrasMap
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
                                           , normalTextureExtras :: Maybe ExtrasMap
                                           }
                       deriving (Show, Generic)

instance FromJSON NormalTextureInfo where
  parseJSON = genericParseJSON $ gltfOptions "normalTexture"

type ImageIndex = Int

data Image = Image { imageName :: Maybe Text
                   , imageUri :: Maybe Text
                   , imageMimeType :: Maybe Text
                   , imageBufferView :: Maybe BufferViewIndex
                   , imageExtras :: Maybe ExtrasMap
                   }
           deriving (Show, Generic)

instance FromJSON Image where
  parseJSON = genericParseJSON $ gltfOptions "image"

data Animation = Animation { animationChannels :: Vector Channel
                           , animationSamplers :: Vector AnimationSampler
                           , animationName :: Maybe Text
                           , animationExtras :: Maybe ExtrasMap
                           }
               deriving (Show, Generic)

instance FromJSON Animation where
  parseJSON = genericParseJSON $ gltfOptions "animation"

data Channel = Channel { channelSampler :: AnimationSamplerIndex
                       , channelTarget :: Target
                       , channelExtras :: Maybe ExtrasMap
                       }
             deriving (Show, Generic)

instance FromJSON Channel where
  parseJSON = genericParseJSON $ gltfOptions "channel"

data TargetPath = TPTranslation | TPRotation | TPScale | TPWeights
                deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON TargetPath where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper . removePrefix "TP"
                                                }

data Target = Target { targetNode :: Maybe NodeIndex
                     , targetPath :: TargetPath
                     , targetExtras :: Maybe ExtrasMap
                     }
             deriving (Show, Generic)

instance FromJSON Target where
  parseJSON = genericParseJSON $ gltfOptions "target"

type AnimationSamplerIndex = Int

data AnimationSamplerInterpolation = ASILinear | ASIStep | ASICubicSpline
                                   deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

instance FromJSON AnimationSamplerInterpolation where
  parseJSON = genericParseJSON $ defaultOptions { constructorTagModifier = map toUpper . removePrefix "ASI"
                                                }

data AnimationSampler = AnimationSampler { animationSamplerInput :: AccessorIndex
                                         , animationSamplerInterpolation :: Maybe AnimationSamplerInterpolation
                                         , animationSamplerOutput :: AccessorIndex
                                         , animationSamplerExtras :: Maybe ExtrasMap
                                         }
                      deriving (Show, Generic)

instance FromJSON AnimationSampler where
  parseJSON = genericParseJSON $ gltfOptions "animationSampler"
