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
  , PipelinePair
  , defaultMaterial
  , newCubePipelineCache
  , loadModel
  ) where

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
import Linear hiding (normalize)
import Graphics.Caramia
import Codec.Picture

import qualified Data.GlTF.Types as TF
import qualified Data.GlTF.Resources as TF
import qualified Data.GlTF.Nodes as TF
import qualified Data.GlTF.Accessors as TF
import Data.GLSL.Preprocessor
import Cube.Types
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.ShadersCache

type NodeName = Text

data LoadedNodeTree = LoadedNodeTree { lnodeTrs :: TRSF
                                     , lnodeMesh :: Maybe LoadedMesh
                                     , lnodeChildren :: Vector LoadedNodeTree
                                     }

data LoadedMesh = LoadedMesh { lmeshPrimitives :: Vector LoadedPrimitive
                             }

data TextureType = BaseColorTexture
                 deriving (Show, Eq, Ord, Bounded, Enum, Generic, Hashable)

type MaterialId = Int

data LoadedPrimitive = LoadedPrimitive { lprimDrawCommand :: DrawCommand
                                       , lprimPipelineId :: PipelineId
                                       , lprimMaterialId :: Maybe MaterialId
                                       }

data LoadedMaterial = LoadedMaterial { lmatTextures :: HashMap TextureType (TF.AttributeSubIndex, Texture)
                                     , lmatBaseColorFactor :: V4 Float
                                     , lmatMetallicFactor :: Float
                                     , lmatRoughnessFactor :: Float
                                     , lmatDoubleSided :: Bool
                                     , lmatAlphaCutoff :: Maybe Float
                                     }

nodeTransform :: TF.Node -> Either String TRSF
nodeTransform (TF.Node { nodeMatrix = Just mtx, nodeRotation = Nothing, nodeScale = Nothing, nodeTranslation = Nothing }) = return $ matrixToTRS mtx
nodeTransform (TF.Node { nodeMatrix = Nothing, .. }) =
  return $ TRS { trsTranslation = fromMaybe (V3 0 0 0) nodeTranslation
               , trsRotation = fromMaybe (Quaternion 1 (V3 0 0 0)) nodeRotation
               , trsScale = fromMaybe (V3 1 1 1) nodeScale
               }
nodeTransform _ = Left "Both transformation matrix and TRS values are specified"

type LoadedBufferViews = HashMap TF.BufferViewIndex LoadedBufferView

data LoadedBufferView = LoadedBufferView { loadedBoundView :: TF.BoundBufferView
                                         , loadedBuffer :: Buffer
                                         }

loadBuffer :: MonadCube m => TF.BoundBufferView -> m LoadedBufferView
loadBuffer loadedBoundView@(TF.BoundBufferView {..}) = do
  loadedBuffer <- newBufferFromBS boundBufferRaw $ \x -> x { accessHints = (Static, Draw)
                                                           , accessFlags = ReadAccess
                                                           }
  return LoadedBufferView {..}

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

prepareAccessor :: LoadedBufferViews -> TF.Accessor -> Either String PreparedAccessor
prepareAccessor bufferViews preparedAccessor@(TF.Accessor { accessorSparse = Nothing, accessorBufferView = Just bufferIndex, .. }) = do
  LoadedBufferView { loadedBoundView = bufferView@TF.BoundBufferView {..}, loadedBuffer = preparedBuffer } <-
    case HM.lookup bufferIndex bufferViews of
      Nothing -> Left [i|Buffer view index is invalid: #{bufferIndex}|]
      Just r -> return r
  -- Validate that buffer view can be bound.
  _bufData <- TF.readRawAccessorWithBuffer bufferView preparedAccessor
  return PreparedAccessor { preparedOffset = fromMaybe 0 accessorByteOffset
                          , preparedStride = fromMaybe 0 $ TF.viewByteStride boundBufferView
                          , ..
                          }
prepareAccessor _ _ = Left "Sparse data in accessors is not supported"

countAttributes :: [Int] -> Either String Int
countAttributes [] = return 0
countAttributes idxs
  | S.toList idxsSet == [0..idxsMax] = return $ idxsMax + 1
  | otherwise = Left "Non-consecutive attribute indices"
  where idxsSet = S.fromList idxs
        Just idxsMax = S.lookupMax idxsSet

textureTypeToDefinitionName :: TextureType -> ByteString
textureTypeToDefinitionName BaseColorTexture = "BASE_COLOR_TEXTURE_IDX"

primitiveToDefinitions :: LoadedMaterial -> Vector PreparedAccessor -> TF.Primitive -> Either String ShaderDefinitions
primitiveToDefinitions (LoadedMaterial {..}) accessors (TF.Primitive {..}) = do
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

  let getTexCoord (typ, (attrIdx, _tex))
        | attrIdx >= texCoordsCount = Nothing
        | otherwise = Just (textureTypeToDefinitionName typ, Just $ B.pack $ show attrIdx)

  let definitions =
           [("HAS_NORMALS", Nothing) | TF.ATNormal `HM.member` primitiveAttributes]
        ++ [("HAS_TANGENTS", Nothing) | TF.ATTangent `HM.member` primitiveAttributes]
        ++ [("HAS_ALPHA_CUTOFF", Nothing) | isJust lmatAlphaCutoff]
        ++ [ ("TEX_COORDS_COUNT", Just $ B.pack $ show texCoordsCount)
           , ("COLORS_COUNT", Just $ B.pack $ show colorsCount)
           , ("JOINTS_COUNT", Just $ B.pack $ show jointsCount)
           , ("WEIGHTS_COUNT", Just $ B.pack $ show weightsCount)
           ]
        ++ zipWith (\(idx :: Int) (count :: Int) -> ([i|COLOR_#{idx}_COMPONENTS|], Just $ B.pack $ show count)) [0..] colorComponents
        ++ mapMaybe getTexCoord (HM.toList lmatTextures)
  return $ HM.fromList definitions

type PipelinePair = (PipelineMeta, LoadedPipeline)

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

loadMaterial :: Vector Texture -> TF.Material -> LoadedMaterial
loadMaterial textures (TF.Material {..}) =
  LoadedMaterial { lmatTextures = primTextures
                 , lmatBaseColorFactor = fromMaybe 1 pbrBaseColorFactor
                 , lmatMetallicFactor = fromMaybe 1 pbrMetallicFactor
                 , lmatRoughnessFactor = fromMaybe 1 pbrRoughnessFactor
                 , lmatDoubleSided = fromMaybe False materialDoubleSided
                 , lmatAlphaCutoff = alphaCutoff
                 }
  where TF.PBRMetallicRoughness {..} = fromMaybe TF.defaultPBRMetallicRoughness materialPbrMetallicRoughness
        primTextures = HM.fromList $ [ (BaseColorTexture, (fromMaybe 0 $ TF.textureInfoTexCoord texInfo, textures V.! TF.textureInfoIndex texInfo)) | texInfo <- toList pbrBaseColorTexture ]
        alphaMode = fromMaybe TF.AMOpaque materialAlphaMode
        alphaCutoff =
          case alphaMode of
            TF.AMMask -> Just $ fromMaybe 0.5 materialAlphaCutoff
            _ -> Nothing

defaultMaterial :: LoadedMaterial
defaultMaterial = loadMaterial V.empty TF.defaultMaterial

loadPrimitive :: MonadCube m => PipelineCache PipelineMeta -> Vector LoadedMaterial -> Vector PreparedAccessor -> TF.Primitive -> StateT (IntMap PipelinePair) m (Maybe LoadedPrimitive)
loadPrimitive plCache materials accessors primitive@(TF.Primitive {..})
  | not (TF.ATPosition `HM.member` primitiveAttributes) = return Nothing
  | otherwise = do
      forM_ (HM.toList primitiveAttributes) $ \(typ, accessorIndex) -> do
        let accessor = accessors V.! accessorIndex
        unless (TF.accessorIsValid typ $ preparedAccessor accessor) $ fail "Invalid accessor for this attribute type"

      let material = maybe defaultMaterial (materials V.!) primitiveMaterial

      definitions <- either fail return $ primitiveToDefinitions material accessors primitive
      (meta, pl) <- getOrCompilePipeline definitions plCache
      modify $ IM.insert (loadedPipelineId pl) (meta, pl)

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

      return $ Just LoadedPrimitive { lprimPipelineId = loadedPipelineId pl
                                    , lprimMaterialId = primitiveMaterial
                                    , lprimDrawCommand = primDrawCommand
                                    }

loadMesh :: MonadCube m => PipelineCache PipelineMeta -> Vector LoadedMaterial -> Vector PreparedAccessor -> TF.Mesh -> StateT (IntMap PipelinePair) m (Maybe LoadedMesh)
loadMesh plCache materials accessors (TF.Mesh {..}) = do
  lmeshPrimitives <- V.fromList <$> catMaybes <$> mapM (loadPrimitive plCache materials accessors) (V.toList meshPrimitives)
  if V.null lmeshPrimitives then
    return Nothing
  else
    return $ Just $ LoadedMesh {..}

data LoadedModel = LoadedModel { loadedNodes :: HashMap NodeName LoadedNodeTree
                               , loadedPipelines :: IntMap PipelinePair
                               , loadedMaterials :: IntMap LoadedMaterial
                               }

instance Semigroup LoadedModel where
  a <> b = LoadedModel { loadedNodes = HM.union (loadedNodes a) (loadedNodes b)
                       , loadedPipelines = IM.union (loadedPipelines a) (loadedPipelines b)
                       , loadedMaterials = IM.union (loadedMaterials a) (loadedMaterials b)
                       }

instance Monoid LoadedModel where
  mempty = LoadedModel { loadedNodes = HM.empty
                       , loadedPipelines = IM.empty
                       , loadedMaterials = IM.empty
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

getPipelineMeta :: LoadedPipeline -> Either String PipelineMeta
getPipelineMeta (LoadedPipeline {..}) = do
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
                                 }
  return $ foldr ($) initialMeta plUniformUpdates

newCubePipelineCache :: MonadCube m => ShaderWithIncludes -> ShaderWithIncludes -> m (PipelineCache PipelineMeta)
newCubePipelineCache = newPipelineCache getPipelineMeta

loadModel :: forall m. MonadCube m => PipelineCache PipelineMeta -> TF.BoundGlTF -> m LoadedModel
loadModel plCache (TF.BoundGlTF {..}) = do
  -- We filter buffer views, because we process those used for images separately.
  let neededBufferViews = S.fromList $ mapMaybe TF.accessorBufferView $ V.toList $ fromMaybe V.empty $ TF.gltfAccessors boundGltf
      runLoadBuffer idx = do
        loaded <- loadBuffer (boundBufferViews V.! idx)
        return (idx, loaded)
  bufferViews <- HM.fromList <$> mapM runLoadBuffer (S.toList neededBufferViews)
  accessors <-
    case mapM (prepareAccessor bufferViews) $ fromMaybe V.empty $ TF.gltfAccessors boundGltf of
      Left e -> fail $ "Failed to validate accessors: " ++ e
      Right r -> return r

  imageBuffers <- mapM loadImageBuffer boundImages
  let samplers = fromMaybe V.empty $ TF.gltfSamplers boundGltf
  textures <- mapM (loadTexture imageBuffers samplers) $ fromMaybe V.empty $ TF.gltfTextures boundGltf

  let materials = fmap (loadMaterial textures) $ fromMaybe V.empty $ TF.gltfMaterials boundGltf
      meshes = fromMaybe V.empty $ TF.gltfMeshes boundGltf

      loadNode :: TF.NodeTree -> StateT (IntMap PipelinePair) m LoadedNodeTree
      loadNode (TF.NodeTree {..}) = do
        lnodeTrs <-
          case nodeTransform nodeTreeNode of
            Left e -> fail $ "Failed to read node transformation values: " ++ e
            Right r -> return r
        let runLoadMesh meshIndex = loadMesh plCache materials accessors mesh
              where mesh = meshes V.! meshIndex
        lnodeMesh <- join <$> mapM runLoadMesh (TF.nodeMesh nodeTreeNode)
        lnodeChildren <- mapM loadNode nodeTreeChildren
        return LoadedNodeTree {..}

      loadTopLevel :: TF.NodeTree -> StateT (IntMap PipelinePair) m (NodeName, LoadedNodeTree)
      loadTopLevel node =
        case TF.nodeName $ TF.nodeTreeNode node of
          Just name -> (name, ) <$> loadNode node
          Nothing -> fail "No name specified for top-level node"

  treeNodes <-
    case TF.gltfNodeTree $ fromMaybe V.empty $ TF.gltfNodes boundGltf of
      Left e -> fail $ "Failed to build node tree: " ++ e
      Right r -> return r
  (nodes, loadedPipelines) <- flip runStateT IM.empty $ mapM loadTopLevel $ V.toList treeNodes
  return LoadedModel { loadedNodes = HM.fromList nodes
                     , loadedMaterials = IM.fromList $ zip [0..] $ V.toList materials
                     , loadedPipelines
                     }
