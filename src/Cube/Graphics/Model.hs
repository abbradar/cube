-- | Load model resources into GPU memory.

{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Cube.Graphics.Model
  ( AnimationName
  , LoadedModel(..)
  , LoadedNodeTree(..)
  , LoadedNode(..)
  , LoadedSampler(..)
  , LoadedSamplerGroup(..)
  , foldMapLoadedSamplerGroup
  , LoadedAnimation(..)
  , EmptySamplerState(..)
  , LoadedMesh(..)
  , LoadedSkin(..)
  , TextureType(..)
  , LoadedPrimitive(..)
  , MaterialId
  , LoadedMaterial(..)
  , ArrayUniform(..)
  , PipelineMeta(..)
  , defaultMaterial
  , CubePipelineCache
  , loadModel
  ) where

import Data.Semigroup ( Max(Max, getMax), Min(Min, getMin) )
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
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
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

import Linear.Matrix.Wrapper
import Linear.VD (VD)
import Data.Vector.Functor
import Data.Vector.Hashable ()
import qualified Data.GlTF.Types as TF
import qualified Data.GlTF.Resources as TF
import qualified Data.GlTF.Nodes as TF
import qualified Data.GlTF.Accessors as TF
import Cube.Types
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.ShadersCache

type AnimationName = Text
type MaterialId = Int


data LoadedModel = LoadedModel { loadedTrees :: Vector LoadedNodeTree
                               , loadedNodes :: Vector LoadedNode
                               , loadedAnimations :: HashMap AnimationName (LoadedAnimation EmptySamplerState)
                               }


data LoadedNodeTree = LoadedNodeTree { lnodeIndex :: TF.NodeIndex
                                     , lnodeChildren :: Vector LoadedNodeTree
                                     }
                      deriving (Show, Eq)

data LoadedNode = LoadedNode { lnodeTrs :: M44F
                             , lnodeMesh :: Maybe LoadedMesh
                             , lnodeSkin :: Maybe LoadedSkin
                             }

newtype LoadedMesh = LoadedMesh { lmeshPrimitives :: Vector LoadedPrimitive
                                }

data LoadedSkin = LoadedSkin { lskinJoints :: Vector TF.NodeIndex
                             , lskinIBM :: VS.Vector M44F
                             }
               --   deriving (Show, Eq)
instance Show LoadedSkin where
  show LoadedSkin{..} = show (length lskinJoints) ++ " " ++ show lskinIBM

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

data EmptySamplerState container component = EmptySamplerState
                                           deriving (Show, Eq, Ord)

data LoadedSampler container component meta = LoadedSampler { lsampInputs :: VS.Vector Float
                                                            , lsampBeginning :: Float
                                                            , lsampEnd :: Float
                                                            , lsampInterpolation :: TF.AnimationSamplerInterpolation
                                                            , lsampOutputs :: container (component Float)
                                                            , lsampMeta :: meta container component
                                                            }

deriving instance (Show (container (component Float)), Show (meta container component)) => Show (LoadedSampler container component meta)

data LoadedSamplerGroup meta = LoadedSamplerGroup { samplerTranslation :: Maybe (LoadedSampler VS.Vector V3 meta)
                                                  , samplerRotation :: Maybe (LoadedSampler VS.Vector Quaternion meta)
                                                  , samplerScale :: Maybe (LoadedSampler VS.Vector V3 meta)
                                                  , samplerWeights :: Maybe (LoadedSampler V.Vector VD meta)
                                                  }

deriving instance ( Show (meta VS.Vector V3)
                  , Show (meta VS.Vector Quaternion)
                  , Show (meta V.Vector VD)
                  ) => Show (LoadedSamplerGroup meta)

emptyLoadedSamplerGroup :: LoadedSamplerGroup meta
emptyLoadedSamplerGroup = LoadedSamplerGroup { samplerTranslation = Nothing
                                             , samplerRotation = Nothing
                                             , samplerScale = Nothing
                                             , samplerWeights = Nothing
                                             }

foldMapLoadedSamplerGroup :: Monoid m => (forall container component. (VG.Vector container (component Float), UnboxFunctor component) => LoadedSampler container component meta -> m) -> LoadedSamplerGroup meta -> m
foldMapLoadedSamplerGroup f (LoadedSamplerGroup {..}) =
  foldMap f samplerTranslation
  <> foldMap f samplerRotation
  <> foldMap f samplerScale
  <> foldMap f samplerWeights

unionLoadedSamplerGroup :: LoadedSamplerGroup meta -> LoadedSamplerGroup meta -> LoadedSamplerGroup meta
unionLoadedSamplerGroup a b = LoadedSamplerGroup { samplerTranslation = liftU2 failUnion (samplerTranslation a) (samplerTranslation b)
                                                 , samplerRotation = liftU2 failUnion (samplerRotation a) (samplerRotation b)
                                                 , samplerScale = liftU2 failUnion (samplerScale a) (samplerScale b)
                                                 , samplerWeights = liftU2 failUnion (samplerWeights a) (samplerWeights b)
                                                 }
  where failUnion _ _ = error "Can't have same animation target specified for the same node"

data LoadedAnimation meta = LoadedAnimation { lanimNodes :: IntMap (LoadedSamplerGroup meta)
                                            , lanimBeginning :: Float
                                            , lanimEnd :: Float
                                            }

deriving instance ( Show (meta VS.Vector V3)
                  , Show (meta VS.Vector Quaternion)
                  , Show (meta V.Vector VD)
                  ) => Show (LoadedAnimation meta)

glQuaternion :: V4F -> QF
glQuaternion (V4 x y z w) = Quaternion w (V3 x y z)

nodeTransform :: TF.Node -> Either String M44F
nodeTransform (TF.Node { nodeMatrix = Just (WM44 mtx), nodeRotation = Nothing, nodeScale = Nothing, nodeTranslation = Nothing }) = return $ transpose mtx
nodeTransform (TF.Node { nodeMatrix = Nothing, .. }) =
  let trs = TRS { trsTranslation = fromMaybe (V3 0 0 0) nodeTranslation
                , trsRotation = maybe (Quaternion 1 0) glQuaternion nodeRotation
                , trsScale = fromMaybe (V3 1 1 1) nodeScale
                }
  in return $ trsToMatrix trs
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

data LoadState = LoadState { currentBufferViews :: IntMap Buffer
                           , currentAccessorsData :: IntMap TF.ConvertedAccessorComponent
                           , currentPreparedAccessors :: IntMap PreparedAccessor
                           }

data LoadInfo = LoadInfo { infoPipelineCache :: CubePipelineCache
                         , infoSourceBuffers :: Vector ByteString
                         , infoBufferViews :: Vector TF.BufferView
                         , infoAccessors :: Vector TF.Accessor
                         }

type ModelT m = StateT LoadState (ReaderT LoadInfo m)

getBufferView :: MonadCube m => TF.BufferViewIndex -> ModelT m Buffer
getBufferView idx = do
  bufferViews <- gets currentBufferViews
  case IM.lookup idx bufferViews of
    Just buf -> return buf
    Nothing -> do
      sourceBuffers <- asks infoSourceBuffers
      sourceBufferViews <- asks infoBufferViews
      bufferData <- either fail return $ TF.getBufferViewBuffer sourceBuffers (sourceBufferViews V.! idx)
      buffer <- newBufferFromBS bufferData $ \x -> x { accessHints = (Static, Draw)
                                                     , accessFlags = ReadAccess
                                                     }
      modify $ \x -> x { currentBufferViews = IM.insert idx buffer bufferViews }
      return buffer

getAccessorData :: MonadCube m => TF.AccessorIndex -> ModelT m TF.ConvertedAccessorComponent
getAccessorData idx = do
  accessors <- gets currentAccessorsData
  case IM.lookup idx accessors of
    Just acc -> return acc
    Nothing -> do
      LoadInfo {..} <- ask
      let accessor = infoAccessors V.! idx
      bufferIndex <-
        case TF.accessorBufferView accessor of
          Nothing -> fail "Sparse data in accessors is not supported"
          Just bidx -> return bidx
      let bufferView = infoBufferViews V.! bufferIndex
      rawAccessorData <- either fail return $ TF.readRawAccessor infoSourceBuffers bufferView accessor
      let accessorData = TF.prepareAccessor accessor rawAccessorData
      modify $ \x -> x { currentAccessorsData = IM.insert idx accessorData accessors }
      return accessorData

getPipeline :: MonadCube m => ShaderKey -> ModelT m (PipelineMeta, LoadedPipeline)
getPipeline key = do
  plCache <- asks infoPipelineCache
  (meta, pl) <- getOrCompilePipeline getPipelineMeta key plCache
  return (meta, pl)

getPreparedAccessor :: MonadCube m => TF.AccessorIndex -> ModelT m PreparedAccessor
getPreparedAccessor idx = do
  accessors <- gets currentPreparedAccessors
  case IM.lookup idx accessors of
    Just acc -> return acc
    Nothing -> do
      LoadInfo {..} <- ask
      let accessor = infoAccessors V.! idx
      bufferViewIndex <-
        case TF.accessorBufferView accessor of
          Nothing -> fail "Sparse data in accessors is not supported"
          Just bidx -> return bidx
      let bufferView = infoBufferViews V.! bufferViewIndex
      unless (TF.accessorIsValid bufferView accessor) $ fail "Buffer view is off buffer bounds"
      preparedView <- getBufferView bufferViewIndex
      let prepared = PreparedAccessor { preparedOffset = fromMaybe 0 (TF.accessorByteOffset accessor)
                                      , preparedStride = fromMaybe 0 (TF.viewByteStride bufferView)
                                      , preparedBuffer = preparedView
                                      , preparedAccessor = accessor
                                      }
      modify $ \x -> x { currentPreparedAccessors = IM.insert idx prepared accessors }
      return prepared

loadAnimation :: forall m. MonadCube m => TF.Animation -> ModelT m (Maybe (AnimationName, LoadedAnimation EmptySamplerState))
loadAnimation (TF.Animation { animationName = Just name, .. }) = do
  nodes <- fmap (IM.fromListWith unionLoadedSamplerGroup . catMaybes) $ mapM loadTarget $ V.toList animationChannels
  let anim = LoadedAnimation { lanimNodes = nodes
                             , lanimBeginning = maybe 0 getMin $ foldMap (foldMapLoadedSamplerGroup (Just . Min . lsampBeginning)) nodes
                             , lanimEnd = maybe 0 getMax $ foldMap (foldMapLoadedSamplerGroup (Just . Max . lsampEnd)) nodes
                             }
  return $ Just (name, anim)

  where loadTarget :: TF.Channel -> ModelT m (Maybe (TF.NodeIndex, LoadedSamplerGroup EmptySamplerState))
        loadTarget (TF.Channel { channelTarget = TF.Target { targetNode = Just nodeIndex, .. }, .. }) = do
          let TF.AnimationSampler {..} = animationSamplers V.! channelSampler
              lsampInterpolation = fromMaybe TF.ASILinear animationSamplerInterpolation
              lsampMeta = EmptySamplerState
          rawInputData <- getAccessorData animationSamplerInput
          inputData <-
            case rawInputData of
              TF.ARFloat (TF.ARScalar x) -> return x
              _ -> fail "Invalid data type for animation sampler input"
          let lsampInputs = TF.convertedVector inputData
          lsampBeginning <- maybe (fail "Min value for input accessor not set") return $ TF.convertedMin $ TF.convertedMinMax inputData
          lsampEnd <- maybe (fail "Max value for input accessor not set") return $ TF.convertedMax $ TF.convertedMinMax inputData
          rawOutputData <- getAccessorData animationSamplerOutput
          group <-
            case targetPath of
              TF.TPTranslation -> do
                lsampOutputs <-
                  case rawOutputData of
                    TF.ARFloat (TF.ARVec3 x) -> return $ TF.convertedVector x
                    _ -> fail "Invalid data type for translation animation sampler output"
                let sampler = LoadedSampler {..}
                return $ emptyLoadedSamplerGroup { samplerTranslation = Just sampler }
              TF.TPRotation -> do
                lsampOutputs <-
                  case rawOutputData of
                    TF.ARFloat (TF.ARVec4 x) -> return $ VS.map glQuaternion $ TF.convertedVector x
                    _ -> fail "Invalid data type for rotation animation sampler output"
                let sampler = LoadedSampler {..}
                return $ emptyLoadedSamplerGroup { samplerRotation = Just sampler }
              TF.TPScale -> do
                lsampOutputs <-
                  case rawOutputData of
                    TF.ARFloat (TF.ARVec3 x) -> return $ TF.convertedVector x
                    _ -> fail "Invalid data type for scale animation sampler output"
                let sampler = LoadedSampler {..}
                return $ emptyLoadedSamplerGroup { samplerScale = Just sampler }
              TF.TPWeights -> fail "FIXME: Morph targets are not yet supported"
          return $ Just (nodeIndex, group)
        loadTarget _ = return Nothing
loadAnimation _ = return Nothing

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

primitiveShaderKey :: MonadCube m => LoadedMaterial -> TF.Primitive -> ModelT m ShaderKey
primitiveShaderKey (LoadedMaterial {..}) (TF.Primitive {..}) = do
  let countAttributeType f = either fail return $ countAttributes $ mapMaybe f $ HM.keys primitiveAttributes
      colorIndices = mapMaybe (\case TF.ATColor idx -> Just idx; _ -> Nothing) $ HM.keys primitiveAttributes

  texCoordsCount <- countAttributeType $ \case TF.ATTexCoord idx -> Just idx; _ -> Nothing
  colorsCount <- either fail return $ countAttributes colorIndices
  jointsCount <- countAttributeType $ \case TF.ATJoints idx -> Just idx; _ -> Nothing
  weightsCount <- countAttributeType $ \case TF.ATWeights idx -> Just idx; _ -> Nothing

  let getColorComponent idx = do
        accessors <- asks infoAccessors
        case TF.accessorType (accessors V.! idx) of
          TF.ATVec3 -> return 3
          TF.ATVec4 -> return 4
          _ -> fail "Invalid accessor type for color attribute"

  colorComponents <- mapM getColorComponent colorIndices

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

loadSkin :: MonadCube m => TF.Skin -> ModelT m (Maybe LoadedSkin)
loadSkin TF.Skin {..} =
  case skinInverseBindMatrices of
    Nothing -> return Nothing
    Just skinIBM' -> do
      rawIBM <- getAccessorData skinIBM'
      lskinIBM' <-
        case rawIBM of
          TF.ARFloat (TF.ARMat4 x) -> return x
          _ -> fail "invalid inverse bind matrices"
      let lskinJoints = skinJoints
      let lskinIBM = TF.convertedVector lskinIBM'
      return $ Just LoadedSkin {..}

loadPrimitive :: MonadCube m => Vector LoadedMaterial -> TF.Primitive -> ModelT m (Maybe LoadedPrimitive)
loadPrimitive materials primitive@(TF.Primitive {..})
  | not (TF.ATPosition `HM.member` primitiveAttributes) = return Nothing
  | otherwise = do
      let material = maybe defaultMaterial (materials V.!) primitiveMaterial

      shaderKey <- primitiveShaderKey material primitive
      (meta, pl) <- getPipeline shaderKey

      primitivesVAO <- newVAO
      (numIndices, sourceData) <-
            case primitiveIndices of
              Nothing -> do
                let accessorIndex = primitiveAttributes HM.! TF.ATPosition
                PreparedAccessor {..} <- getPreparedAccessor accessorIndex
                unless (TF.attributeAccessorIsValid TF.ATPosition preparedAccessor) $ fail "Invalid accessor for this attribute type"

                let src = Primitives { firstIndex = 0 }
                return (TF.accessorCount preparedAccessor, src)
              Just indicesIndex -> do
                PreparedAccessor {..} <- getPreparedAccessor indicesIndex
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
        PreparedAccessor { preparedAccessor = accessor@TF.Accessor {..}, ..} <- getPreparedAccessor accessorIndex
        unless (TF.attributeAccessorIsValid typ $ accessor) $ fail "Invalid accessor for this attribute type"

        let sourcing = defaultSourcing { offset = preparedOffset
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

loadMesh :: MonadCube m => Vector LoadedMaterial -> TF.Mesh -> ModelT m (Maybe LoadedMesh)
loadMesh materials (TF.Mesh {..}) = do
  lmeshPrimitives <- V.fromList <$> catMaybes <$> mapM (loadPrimitive materials) (V.toList meshPrimitives)
  if V.null lmeshPrimitives then
    return Nothing
  else
    return $ Just $ LoadedMesh {..}

data ArrayUniform = ArrayUniform { arrayIndex :: UniformLocation
                                 , arraySize :: Int
                                 }
                  deriving (Show, Eq)

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
                                 , pipelineBoneMatrices :: Maybe ArrayUniform
                                 , pipelineOffsetMatrices :: Maybe ArrayUniform
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
          | uniformSize /= 1 =
            case uniformName of
              "uniBoneMatrices[0]" -> return $ \x -> x { pipelineBoneMatrices = Just ArrayUniform { arrayIndex = idx, arraySize = uniformSize } }
              "uniOffsetMatrices[0]" -> return $ \x -> x { pipelineOffsetMatrices = Just ArrayUniform { arrayIndex = idx, arraySize = uniformSize } }
              _ -> Left [i|Unknown array uniform #{uniformName}|]
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
                                       , pipelineBoneMatrices = Nothing
                                       , pipelineOffsetMatrices = Nothing
                                       , pipelineId = plId
                                       }
        return $ foldr ($) initialMeta plUniformUpdates
  either fail return run


loadModel' :: forall m. MonadCube m => TF.BoundGlTF -> ModelT m (Vector LoadedNodeTree, Vector LoadedNode, HashMap AnimationName (LoadedAnimation EmptySamplerState))
loadModel' (TF.BoundGlTF {..}) = do
  imageBuffers <- mapM loadImageBuffer boundImages
  let samplers = fromMaybe V.empty $ TF.gltfSamplers boundGltf
  textures <- mapM (loadTexture imageBuffers samplers) $ fromMaybe V.empty $ TF.gltfTextures boundGltf

  let materials = V.imap (loadMaterial textures) $ fromMaybe V.empty $ TF.gltfMaterials boundGltf
      meshes = fromMaybe V.empty $ TF.gltfMeshes boundGltf
      skins = fromMaybe V.empty $ TF.gltfSkins boundGltf

      loadNode :: TF.Node -> StateT LoadState (ReaderT LoadInfo m) LoadedNode
      loadNode node = do
        lnodeTrs <-
          case nodeTransform node of
            Left e -> fail $ "Failed to read node transformation values: " ++ e
            Right r -> return r
        lnodeMesh <- case TF.nodeMesh node of
          Nothing -> return Nothing
          Just meshIndex -> maybe (fail $ "no required mesh: " ++ show meshIndex) (loadMesh materials) (meshes V.!? meshIndex)
        lnodeSkin <- case TF.nodeSkin node of
          Nothing -> return Nothing
          Just skinIndex -> maybe (fail $ "no required mesh: " ++ show skinIndex) loadSkin (skins V.!? skinIndex)
        return LoadedNode{..}

      loadNodeTree :: TF.NodeTree -> LoadedNodeTree
      loadNodeTree TF.NodeTree {..} = LoadedNodeTree { lnodeIndex = nodeTreeIndex, lnodeChildren = fmap loadNodeTree nodeTreeChildren }

  (treeNodes, nodes') <-
    case TF.gltfNodeTree $ fromMaybe V.empty $ TF.gltfNodes boundGltf of
      Left e -> fail $ "Failed to build node tree: " ++ e
      Right r -> return (r, fromMaybe V.empty $ TF.gltfNodes boundGltf)

  nodes <- mapM loadNode nodes'
  let trees = fmap loadNodeTree treeNodes

  animations <- fmap (HM.fromList . catMaybes) $ mapM loadAnimation $ V.toList $ fromMaybe V.empty $ TF.gltfAnimations boundGltf
  return (trees, nodes, animations)

loadModel :: forall m. MonadCube m => PipelineCache ShaderKey PipelineMeta -> TF.BoundGlTF -> m LoadedModel
loadModel plCache bound = do
  let initial = LoadState { currentBufferViews = IM.empty
                          , currentAccessorsData = IM.empty
                          , currentPreparedAccessors = IM.empty
                          }
      info = LoadInfo { infoPipelineCache = plCache
                      , infoSourceBuffers = TF.boundBuffers bound
                      , infoBufferViews = fromMaybe V.empty $ TF.gltfBufferViews $ TF.boundGltf bound
                      , infoAccessors = fromMaybe V.empty $ TF.gltfAccessors $ TF.boundGltf bound
                      }
  (trees, nodes, animations) <- flip runReaderT info $ flip evalStateT initial $ loadModel' bound
  return LoadedModel { loadedTrees = trees
                     , loadedNodes = nodes
                     , loadedAnimations = animations
                     }
