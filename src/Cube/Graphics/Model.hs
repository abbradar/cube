-- | Load model resources into GPU memory.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Model
  ( NodeName
  , LoadedModel(..)
  , LoadedNodeTree(..)
  , LoadedMesh(..)
  , TextureType(..)
  , LoadedPrimitive(..)
  , MaterialId
  , LoadedMaterial(..)
  , PipelineMeta(..)
  , defaultMaterial
  , CubePipelineCache
  , loadModel
  ) where

import Data.Typeable
import Data.Functor
import Data.Maybe
import Data.Foldable
import Data.Hashable
import GHC.Generics (Generic)
import Control.Applicative
import Control.Monad
import Data.String.Interpolate
import Data.Text (Text)
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Attoparsec.ByteString.Char8 as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Control.Monad.State.Strict
import Control.Monad.Reader
import Linear hiding (normalize)
import Graphics.Caramia
import Codec.Picture
import Graphics.Caramia.OpenGLResource

import Data.Vector.Hashable ()
import qualified Data.GlTF.Types as TF
import qualified Data.GlTF.Resources as TF
import qualified Data.GlTF.Nodes as TF
import Cube.Types
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.ShadersCache

type NodeName = Text
type MaterialId = Int

data LoadedNodeTree = LoadedNodeTree { lnodeTrs :: TRSF
                                     , lnodeMesh :: Maybe LoadedMesh
                                     , lnodeIndex :: TF.NodeIndex
                                     , lnodeChildren :: Vector LoadedNodeTree
                                     }

data LoadedMesh = LoadedMesh { lmeshPrimitives :: Vector LoadedPrimitive
                             }

data TextureType = BaseColorTexture
                 deriving (Show, Eq, Ord, Bounded, Enum, Generic, Typeable, Hashable)

data LoadedPrimitive = LoadedPrimitive { lprimDrawCommand :: DrawCommand
                                       , lprimPipelineMeta :: PipelineMeta
                                       , lprimPipeline :: LoadedPipeline
                                       , lprimMaterial :: LoadedMaterial
                                       }

data LoadedMaterial = LoadedMaterial { lmatTextures :: HashMap TextureType (TF.AttributeSubIndex, Texture)
                                     , lmatBaseColorFactor :: V4F
                                     , lmatMetallicFactor :: Float
                                     , lmatRoughnessFactor :: Float
                                     , lmatDoubleSided :: Bool
                                     , lmatAlphaCutoff :: Maybe Float
                                     , lmatId :: MaterialId
                                     }

nodeTransform :: TF.Node -> Either String TRSF
nodeTransform (TF.Node { nodeMatrix = Just mtx, nodeRotation = Nothing, nodeScale = Nothing, nodeTranslation = Nothing }) = return $ matrixToTRS mtx
nodeTransform (TF.Node { nodeMatrix = Nothing, .. }) =
  return $ TRS { trsTranslation = fromMaybe (V3 0 0 0) nodeTranslation
               , trsRotation = fromMaybe (Quaternion 1 (V3 0 0 0)) nodeRotation
               , trsScale = fromMaybe (V3 1 1 1) nodeScale
               }
nodeTransform _ = Left "Both transformation matrix and TRS values are specified"

imageHasAlpha :: DynamicImage -> Bool
imageHasAlpha (ImageY8 _) = False
imageHasAlpha (ImageY16 _) = False
imageHasAlpha (ImageY32 _) = False
imageHasAlpha (ImageYF _) = False
imageHasAlpha (ImageYA8 _) = True
imageHasAlpha (ImageYA16 _) = True
imageHasAlpha (ImageRGB8 _) = False
imageHasAlpha (ImageRGB16 _) = False
imageHasAlpha (ImageRGBF _) = False
imageHasAlpha (ImageRGBA8 _) = True
imageHasAlpha (ImageRGBA16 _) = True
imageHasAlpha (ImageYCbCr8 _) = False
imageHasAlpha (ImageCMYK8 _) = False
imageHasAlpha (ImageCMYK16 _) = False

loadImageBuffer :: MonadCube m => DynamicImage -> m (Topology, Uploading)
loadImageBuffer dynImg
  | imageHasAlpha dynImg = make URGBA $ convertRGBA8 dynImg
  | otherwise = make URGB $ convertRGB8 dynImg

  where make format (Image {..}) = do
          imgBuffer <- newBufferFromVector imageData $ \x -> x { accessHints = (Static, Draw)
                                                               , accessFlags = ReadAccess
                                                               }
          let upl = uploading2D imgBuffer imageWidth imageHeight FWord8 format
          return (Tex2D imageWidth imageHeight, upl)

defaultImageFormat :: UploadFormat -> ImageFormat
defaultImageFormat UR = R32F
defaultImageFormat URG = RG32F
defaultImageFormat URGB = COMPRESSED_RGB_S3TC_DXT1
defaultImageFormat URGBA = COMPRESSED_RGBA_S3TC_DXT5
defaultImageFormat UBGR = COMPRESSED_RGB_S3TC_DXT1
defaultImageFormat UBGRA = COMPRESSED_RGBA_S3TC_DXT5
defaultImageFormat UDEPTH_COMPONENT = DEPTH_COMPONENT32F
defaultImageFormat USTENCIL_INDEX = DEPTH32F_STENCIL8

convertWrapping :: TF.WrappingMode -> Wrapping
convertWrapping TF.WMClampToEdge = Clamp
convertWrapping TF.WMMirroredRepeat = MirroredRepeat
convertWrapping TF.WMRepeat = Repeat

convertMinFilter :: TF.MinFilter -> MinFilter
convertMinFilter TF.MinNearest = MiNearest
convertMinFilter TF.MinLinear = MiLinear
convertMinFilter TF.MinNearestMipmapNearest = MiNearestMipmapNearest
convertMinFilter TF.MinLinearMipmapNearest = MiLinearMipmapNearest
convertMinFilter TF.MinNearestMipmapLinear = MiNearestMipmapLinear
convertMinFilter TF.MinLinearMipmapLinear = MiLinearMipmapLinear

needsMipmaps :: TF.MinFilter -> Bool
needsMipmaps TF.MinNearest = False
needsMipmaps TF.MinLinear = False
needsMipmaps TF.MinNearestMipmapNearest = True
needsMipmaps TF.MinLinearMipmapNearest = True
needsMipmaps TF.MinNearestMipmapLinear = True
needsMipmaps TF.MinLinearMipmapLinear = True

convertMagFilter :: TF.MagFilter -> MagFilter
convertMagFilter TF.MagNearest = MaNearest
convertMagFilter TF.MagLinear = MaLinear

loadTexture :: MonadCube m => Vector (Topology, Uploading) -> Vector TF.Sampler -> TF.Texture -> m Texture
loadTexture images samplers (TF.Texture {..}) = do
  texIndex <- maybe (fail "No texture source specified") return textureSource
  let (topology, uploading) = images V.! texIndex
  let TF.Sampler {..} = maybe TF.defaultSampler (samplers V.!) textureSampler

  tex <- newTexture $ textureSpecification { topology = topology
                                           , imageFormat = defaultImageFormat $ uploadFormat uploading
                                           }
  uploadToTexture uploading tex

  let minFilter = fromMaybe TF.MinNearestMipmapLinear samplerMinFilter
  case minFilter of
    TF.MinNearestMipmapLinear -> return ()
    f -> setMinFilter (convertMinFilter f) tex
  when (needsMipmaps minFilter) $ generateMipmaps tex

  case fromMaybe TF.MagLinear samplerMagFilter of
    TF.MagLinear -> return ()
    magFilter -> setMagFilter (convertMagFilter magFilter) tex

  case fromMaybe TF.WMRepeat samplerWrapS of
    TF.WMRepeat -> return ()
    wrapS -> setWrapS (convertWrapping wrapS) tex

  case fromMaybe TF.WMRepeat samplerWrapT of
    TF.WMRepeat -> return ()
    wrapT -> setWrapT (convertWrapping wrapT) tex

  return tex

data PreparedAccessor = PreparedAccessor { preparedAccessor :: TF.Accessor
                                         , preparedBuffer :: Buffer
                                         , preparedStride :: Int
                                         , preparedOffset :: Int
                                         }

type CubePipelineCache = PipelineCache ShaderKey PipelineMeta

newtype LoadState = LoadState { currentBuffers :: IntMap Buffer
                              }

data LoadInfo = LoadInfo { infoPipelineCache :: CubePipelineCache
                         , infoSourceBuffers :: Vector ByteString
                         }

type ModelT m = StateT LoadState (ReaderT LoadInfo m)

getBuffer :: MonadCube m => Int -> ModelT m Buffer
getBuffer idx = do
  buffers <- currentBuffers <$> get
  case IM.lookup idx buffers of
    Just buf -> return buf
    Nothing -> do
      sourceBuffers <- asks infoSourceBuffers
      buffer <- newBufferFromBS (sourceBuffers V.! idx) $ \x -> x { accessHints = (Static, Draw)
                                                                  , accessFlags = ReadAccess
                                                                  }
      modify $ \x -> x { currentBuffers = IM.insert idx buffer buffers }
      return buffer

getPipeline :: MonadCube m => ShaderKey -> ModelT m (PipelineMeta, LoadedPipeline)
getPipeline key = do
  plCache <- asks infoPipelineCache
  (meta, pl) <- getOrCompilePipeline getPipelineMeta key plCache
  return (meta, pl)

prepareAccessor :: MonadCube m => Vector TF.BufferView -> TF.Accessor -> ModelT m PreparedAccessor
prepareAccessor bufferViews preparedAccessor@(TF.Accessor { accessorSparse = Nothing, accessorBufferView = Just bufferIndex, .. }) = do
  let bufferView@TF.BufferView {..} = bufferViews V.! bufferIndex
  -- Validate that buffer view can be bound.
  unless (TF.accessorIsValid bufferView preparedAccessor) $ fail "Buffer view is off buffer bounds"
  buffer <- getBuffer viewBuffer
  return $ PreparedAccessor { preparedOffset = fromMaybe 0 viewByteOffset + fromMaybe 0 accessorByteOffset
                            , preparedStride = fromMaybe 0 viewByteStride
                            , preparedBuffer = buffer
                            , preparedAccessor
                            }
prepareAccessor _ _ = fail "Sparse data in accessors is not supported"

countAttributes :: [Int] -> Either String Int
countAttributes [] = return 0
countAttributes idxs
  | S.toList idxsSet == [0..idxsMax] = return $ idxsMax + 1
  | otherwise = Left "Non-consecutive attribute indices"
  where idxsSet = S.fromList idxs
        Just idxsMax = S.lookupMax idxsSet

textureTypeToDefinitionName :: TextureType -> ByteString
textureTypeToDefinitionName BaseColorTexture = "BASE_COLOR_TEXTURE_IDX"

data ShaderKey = ShaderKey { shaderHasNormals :: Bool
                           , shaderHasTangents :: Bool
                           , shaderHasAlphaCutoff :: Bool
                           , shaderTexCoordsCount :: Int
                           , shaderColorsCount :: Int
                           , shaderJointsCount :: Int
                           , shaderWeightsCount :: Int
                           , shaderColorComponents :: Vector Int
                           , shaderTextures :: HashMap TextureType Int
                           }
               deriving (Show, Eq, Ord, Generic, Typeable, Hashable)

primitiveShaderKey :: LoadedMaterial -> Vector PreparedAccessor -> TF.Primitive -> Either String ShaderKey
primitiveShaderKey (LoadedMaterial {..}) accessors (TF.Primitive {..}) = do
  let countAttributeType f = countAttributes $ mapMaybe f $ HM.keys primitiveAttributes
      colorIndices = mapMaybe (\case TF.ATColor idx -> Just idx; _ -> Nothing) $ HM.keys primitiveAttributes

  texCoordsCount <- countAttributeType $ \case TF.ATTexCoord idx -> Just idx; _ -> Nothing
  colorsCount <- countAttributes colorIndices
  jointsCount <- countAttributeType $ \case TF.ATJoints idx -> Just idx; _ -> Nothing
  weightsCount <- countAttributeType $ \case TF.ATWeights idx -> Just idx; _ -> Nothing

  let vecCount TF.ATVec3 = Right 3
      vecCount TF.ATVec4 = Right 4
      vecCount _ = Left "Invalid accessor type for color attribute"
  colorComponents <- mapM (\idx -> vecCount $ TF.accessorType $ preparedAccessor $ accessors V.! idx) colorIndices

  let getTexCoord (attrIdx, _tex)
        | attrIdx >= texCoordsCount = Nothing
        | otherwise = Just attrIdx

  return $ ShaderKey { shaderHasNormals = TF.ATNormal `HM.member` primitiveAttributes
                     , shaderHasTangents = TF.ATTangent `HM.member` primitiveAttributes
                     , shaderHasAlphaCutoff = isJust lmatAlphaCutoff
                     , shaderTexCoordsCount = texCoordsCount
                     , shaderColorsCount = colorsCount
                     , shaderJointsCount = jointsCount
                     , shaderWeightsCount = weightsCount
                     , shaderColorComponents = V.fromList colorComponents
                     , shaderTextures = HM.mapMaybe getTexCoord lmatTextures
                     }

instance ToShaderDefinitions ShaderKey where
  toShaderDefinitions (ShaderKey {..}) =
    HM.fromList $
    [("HAS_NORMALS", Nothing) | shaderHasNormals]
    ++ [("HAS_TANGENTS", Nothing) | shaderHasTangents]
    ++ [("HAS_ALPHA_CUTOFF", Nothing) | shaderHasAlphaCutoff]
    ++ [ ("TEX_COORDS_COUNT", Just $ B.pack $ show shaderTexCoordsCount)
       , ("COLORS_COUNT", Just $ B.pack $ show shaderColorsCount)
       , ("JOINTS_COUNT", Just $ B.pack $ show shaderJointsCount)
       , ("WEIGHTS_COUNT", Just $ B.pack $ show shaderWeightsCount)
       ]
    ++ zipWith (\(idx :: Int) (count :: Int) -> ([i|COLOR_#{idx}_COMPONENTS|], Just $ B.pack $ show count)) [0..] (V.toList shaderColorComponents)
    ++ map (\(typ, attrIdx) -> (textureTypeToDefinitionName typ, Just $ B.pack $ show attrIdx)) (HM.toList shaderTextures)

convertPrimitiveMode :: TF.PrimitiveMode -> Primitive
convertPrimitiveMode TF.PMPoints = Points
convertPrimitiveMode TF.PMLines = Lines
convertPrimitiveMode TF.PMLineLoop = LineLoop
convertPrimitiveMode TF.PMLineStrip = LineStrip
convertPrimitiveMode TF.PMTriangles = Triangles
convertPrimitiveMode TF.PMTriangleStrip = TriangleStrip
convertPrimitiveMode TF.PMTriangleFan = TriangleFan

convertAccessorComponentType :: TF.ComponentType -> SourceType
convertAccessorComponentType TF.CTByte = SInt8
convertAccessorComponentType TF.CTUnsignedByte = SWord8
convertAccessorComponentType TF.CTShort = SInt16
convertAccessorComponentType TF.CTUnsignedShort = SWord16
convertAccessorComponentType TF.CTUnsignedInt = SWord32
convertAccessorComponentType TF.CTFloat = SFloat

loadMaterial :: Vector Texture -> TF.MaterialIndex -> TF.Material -> LoadedMaterial
loadMaterial textures index (TF.Material {..}) =
  LoadedMaterial { lmatTextures = primTextures
                 , lmatBaseColorFactor = fromMaybe 1 pbrBaseColorFactor
                 , lmatMetallicFactor = fromMaybe 1 pbrMetallicFactor
                 , lmatRoughnessFactor = fromMaybe 1 pbrRoughnessFactor
                 , lmatDoubleSided = fromMaybe False materialDoubleSided
                 , lmatAlphaCutoff = alphaCutoff
                 , lmatId = index
                 }
  where TF.PBRMetallicRoughness {..} = fromMaybe TF.defaultPBRMetallicRoughness materialPbrMetallicRoughness
        primTextures = HM.fromList $ [ (BaseColorTexture, (fromMaybe 0 $ TF.textureInfoTexCoord texInfo, textures V.! TF.textureInfoIndex texInfo)) | texInfo <- toList pbrBaseColorTexture ]
        alphaMode = fromMaybe TF.AMOpaque materialAlphaMode
        alphaCutoff =
          case alphaMode of
            TF.AMMask -> Just $ fromMaybe 0.5 materialAlphaCutoff
            _ -> Nothing

defaultMaterial :: LoadedMaterial
defaultMaterial = loadMaterial V.empty (-1) TF.defaultMaterial

loadPrimitive :: MonadCube m => Vector LoadedMaterial -> Vector PreparedAccessor -> TF.Primitive -> ModelT m (Maybe LoadedPrimitive)
loadPrimitive materials accessors primitive@(TF.Primitive {..})
  | not (TF.ATPosition `HM.member` primitiveAttributes) = return Nothing
  | otherwise = do
      forM_ (HM.toList primitiveAttributes) $ \(typ, accessorIndex) -> do
        let accessor = accessors V.! accessorIndex
        unless (TF.attributeAccessorIsValid typ $ preparedAccessor accessor) $ fail "Invalid accessor for this attribute type"

      let material = maybe defaultMaterial (materials V.!) primitiveMaterial

      shaderKey <- either fail return $ primitiveShaderKey material accessors primitive
      (meta, pl) <- getPipeline shaderKey

      primitivesVAO <- newVAO
      (numIndices, sourceData) <-
            case primitiveIndices of
              Nothing ->
                let PreparedAccessor {..} = accessors V.! (primitiveAttributes HM.! TF.ATPosition)
                    src = Primitives { firstIndex = 0 }
                in return (TF.accessorCount preparedAccessor, src)
              Just indicesIndex -> do
                let PreparedAccessor {..} = accessors V.! indicesIndex
                unless (preparedStride == 0) $ fail "Strides for indice accessors are not supported"
                indicesType <-
                  case TF.accessorComponentType preparedAccessor of
                    TF.CTUnsignedByte -> return IWord8
                    TF.CTUnsignedShort -> return IWord16
                    TF.CTUnsignedInt -> return IWord32
                    invalidTyp -> fail $ [i|Invalid accessor #{indicesIndex} type for indices: #{invalidTyp}|]
                let src = PrimitivesWithIndices { indexBuffer = preparedBuffer
                                                , indexOffset = preparedOffset
                                                , indexType = indicesType
                                                }
                return (TF.accessorCount preparedAccessor, src)
      let primDrawCommand = drawCommand { primitiveType = convertPrimitiveMode $ fromMaybe TF.PMTriangles primitiveMode
                                        , primitivesVAO
                                        , numIndices
                                        , sourceData
                                        }

      forM_ (HM.toList $ pipelineAttributes meta) $ \(typ, plAttrIndex) -> do
        let accessorIndex = primitiveAttributes HM.! typ
            PreparedAccessor { preparedAccessor = TF.Accessor {..}, ..} = accessors V.! accessorIndex
            sourcing = defaultSourcing { offset = preparedOffset
                                       , components = TF.accessorComponentsNumber accessorType
                                       , stride = preparedStride
                                       , normalize = fromMaybe False accessorNormalized
                                       , sourceType = convertAccessorComponentType accessorComponentType
                                       , attributeIndex = plAttrIndex
                                       , integerMapping = TF.attributeIsIntegral typ
                                       }

        sourceVertexData preparedBuffer sourcing primitivesVAO

      return $ Just LoadedPrimitive { lprimPipelineMeta = meta
                                    , lprimPipeline = pl
                                    , lprimMaterial = material
                                    , lprimDrawCommand = primDrawCommand
                                    }

loadMesh :: MonadCube m => Vector LoadedMaterial -> Vector PreparedAccessor -> TF.Mesh -> ModelT m (Maybe LoadedMesh)
loadMesh materials accessors (TF.Mesh {..}) = do
  lmeshPrimitives <- V.fromList <$> catMaybes <$> mapM (loadPrimitive materials accessors) (V.toList meshPrimitives)
  if V.null lmeshPrimitives then
    return Nothing
  else
    return $ Just $ LoadedMesh {..}

data LoadedModel = LoadedModel { loadedNodes :: Vector LoadedNodeTree
                               }

data PipelineMeta = PipelineMeta { pipelineAttributes :: HashMap TF.AttributeType AttributeLocation
                                 , pipelineViewProjectionMatrix :: Maybe UniformLocation
                                 , pipelineModelMatrix :: Maybe UniformLocation
                                 , pipelineNormalMatrix :: Maybe UniformLocation
                                 , pipelineBaseColorFactor :: Maybe UniformLocation
                                 , pipelineMetallicFactor :: Maybe UniformLocation
                                 , pipelineRoughnessFactor :: Maybe UniformLocation
                                 , pipelineAlphaCutoff :: Maybe UniformLocation
                                 , pipelineCamera :: Maybe UniformLocation
                                 , pipelineTextures :: HashMap TextureType UniformLocation
                                 , pipelineId :: PipelineId -- Used for fast indexing
                                 }
                  deriving (Show, Eq)

prefixedVariable :: ByteString -> (Int -> TF.AttributeType) -> Atto.Parser TF.AttributeType
prefixedVariable prefix constr = do
  _ <- Atto.string prefix
  idx <- (Atto.char '_' *> Atto.decimal) <|> pure 0
  return $ constr idx

attributeVariableParser :: Atto.Parser TF.AttributeType
attributeVariableParser =
      (Atto.string "attrPosition" $> TF.ATPosition)
  <|> (Atto.string "attrNormal" $> TF.ATNormal)
  <|> (Atto.string "attrTangent" $> TF.ATTangent)
  <|> prefixedVariable "attrTexCoord" TF.ATTexCoord
  <|> prefixedVariable "attrColor" TF.ATColor
  <|> prefixedVariable "attrJoint" TF.ATJoints
  <|> prefixedVariable "attrWeight" TF.ATWeights

getPipelineAttributes :: HashMap AttributeName (AttributeLocation, AttributeInfo) -> Either String (HashMap TF.AttributeType AttributeLocation)
getPipelineAttributes loadedAttributes = HM.fromList <$> mapM mapAttribute (HM.elems loadedAttributes)
  where mapAttribute (idx, AttributeInfo {..})
          | attributeSize /= 1 = Left [i|Array attributes are not supported: #{attributeName}|]
          | otherwise = do
              case Atto.parseOnly (attributeVariableParser <* Atto.endOfInput) attributeName of
                Left e -> Left [i|Couldn't parse attribute name #{attributeName}: #{e}|]
                Right r -> return (r, idx)

getPipelineUniforms :: HashMap UniformName (UniformLocation, UniformInfo) -> Either String [PipelineMeta -> PipelineMeta]
getPipelineUniforms loadedUniforms = mapM mapAttribute (HM.elems loadedUniforms)
  where mapAttribute (idx, UniformInfo {..})
          | uniformSize /= 1 = Left "Array uniforms are not supported"
          | otherwise =
            case uniformName of
              "uniViewProjectionMatrix" -> return $ \x -> x { pipelineViewProjectionMatrix = Just idx }
              "uniModelMatrix" -> return $ \x -> x { pipelineModelMatrix = Just idx }
              "uniNormalMatrix" -> return $ \x -> x { pipelineNormalMatrix = Just idx }
              "uniBaseColorFactor" -> return $ \x -> x { pipelineBaseColorFactor = Just idx }
              "uniBaseColorTexture" -> return $ \x -> x { pipelineTextures = HM.insert BaseColorTexture idx $ pipelineTextures x }
              "uniMetallicFactor" -> return $ \x -> x { pipelineMetallicFactor = Just idx }
              "uniRoughnessFactor" -> return $ \x -> x { pipelineRoughnessFactor = Just idx }
              "uniAlphaCutoff" -> return $ \x -> x { pipelineAlphaCutoff = Just idx }
              "uniCamera" -> return $ \x -> x { pipelineCamera = Just idx }
              _ -> Left [i|Unknown uniform #{uniformName}|]

getPipelineMeta :: MonadCube m => LoadedPipeline -> m PipelineMeta
getPipelineMeta (LoadedPipeline {..}) = do
  plId <- fromIntegral <$> getRaw loadedPipeline
  let run = do
        plAttributes <- getPipelineAttributes loadedAttributes
        plUniformUpdates <- getPipelineUniforms loadedUniforms
        let initialMeta = PipelineMeta { pipelineAttributes = plAttributes
                                       , pipelineViewProjectionMatrix = Nothing
                                       , pipelineModelMatrix = Nothing
                                       , pipelineNormalMatrix = Nothing
                                       , pipelineBaseColorFactor = Nothing
                                       , pipelineMetallicFactor = Nothing
                                       , pipelineRoughnessFactor = Nothing
                                       , pipelineAlphaCutoff = Nothing
                                       , pipelineCamera = Nothing
                                       , pipelineTextures = HM.empty
                                       , pipelineId = plId
                                       }
        return $ foldr ($) initialMeta plUniformUpdates
  either fail return run

loadModel' :: forall m. MonadCube m => TF.BoundGlTF -> ModelT m (Vector LoadedNodeTree)
loadModel' (TF.BoundGlTF {..}) = do
  let bufferViews = fromMaybe V.empty $ TF.gltfBufferViews boundGltf
  accessors <- mapM (prepareAccessor bufferViews) $ fromMaybe V.empty $ TF.gltfAccessors boundGltf

  imageBuffers <- mapM loadImageBuffer boundImages
  let samplers = fromMaybe V.empty $ TF.gltfSamplers boundGltf
  textures <- mapM (loadTexture imageBuffers samplers) $ fromMaybe V.empty $ TF.gltfTextures boundGltf

  let materials = V.imap (loadMaterial textures) $ fromMaybe V.empty $ TF.gltfMaterials boundGltf
      meshes = fromMaybe V.empty $ TF.gltfMeshes boundGltf

      loadNode (TF.NodeTree {..}) = do
        lnodeTrs <-
          case nodeTransform nodeTreeNode of
            Left e -> fail $ "Failed to read node transformation values: " ++ e
            Right r -> return r
        let runLoadMesh meshIndex = loadMesh materials accessors mesh
              where mesh = meshes V.! meshIndex
        lnodeMesh <- join <$> mapM runLoadMesh (TF.nodeMesh nodeTreeNode)
        lnodeChildren <- mapM loadNode nodeTreeChildren
        return LoadedNodeTree { lnodeIndex = nodeTreeIndex
                              , ..
                              }

  treeNodes <-
    case TF.gltfNodeTree $ fromMaybe V.empty $ TF.gltfNodes boundGltf of
      Left e -> fail $ "Failed to build node tree: " ++ e
      Right r -> return r
  mapM loadNode treeNodes

loadModel :: forall m. MonadCube m => PipelineCache ShaderKey PipelineMeta -> TF.BoundGlTF -> m LoadedModel
loadModel plCache bound = do
  let initial = LoadState { currentBuffers = IM.empty
                          }
      info = LoadInfo { infoPipelineCache = plCache
                      , infoSourceBuffers = TF.boundBuffers bound
                      }
  nodes <- flip runReaderT info $ flip evalStateT initial $ loadModel' bound
  return LoadedModel { loadedNodes = nodes
                     }
