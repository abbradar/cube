-- | Load resources into GPU memory.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Resources
  ( NodeName
  , LoadedNodes(..)
  , LoadedNodeTree(..)
  , LoadedMesh(..)
  , LoadedPrimitive(..)
  , PipelineMeta(..)
  , PipelinePair
  , newCubePipelineCache
  , loadNodes
  ) where

import Data.Functor
import Data.Maybe
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
import qualified Data.HashMap.Strict as HMS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IMS
import qualified Data.Set as S
import Control.Monad.State.Strict
import Linear hiding (normalize)
import Graphics.Caramia

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

data LoadedPrimitive = LoadedPrimitive { lprimDrawCommand :: DrawCommand
                                       , lprimPipelineId :: Int
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

data PreparedAccessor = PreparedAccessor { preparedAccessor :: TF.Accessor
                                         , preparedBuffer :: Buffer
                                         , preparedStride :: Int
                                         , preparedOffset :: Int
                                         }

prepareAccessor :: LoadedBufferViews -> TF.Accessor -> Either String PreparedAccessor
prepareAccessor bufferViews preparedAccessor@(TF.Accessor { accessorSparse = Nothing, accessorBufferView = Just bufferIndex, .. }) = do
  LoadedBufferView { loadedBoundView = bufferView@TF.BoundBufferView {..}, loadedBuffer = preparedBuffer } <-
    case HMS.lookup bufferIndex bufferViews of
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

primitiveToDefinitions :: Vector TF.Material -> Vector PreparedAccessor -> TF.Primitive -> ShaderDefinitions
primitiveToDefinitions _materials accessors (TF.Primitive {..}) = HMS.fromList definitions
  where countAttributeType f = countAttributes $ mapMaybe f $ HMS.keys primitiveAttributes
        colorIndices = mapMaybe (\case TF.Color idx -> Just idx; _ -> Nothing) $ HMS.keys primitiveAttributes

        texCoordsCount = countAttributeType $ \case TF.TexCoord idx -> Just idx; _ -> Nothing
        colorsCount = countAttributes colorIndices
        jointsCount = countAttributeType $ \case TF.Joints idx -> Just idx; _ -> Nothing
        weightsCount = countAttributeType $ \case TF.Weights idx -> Just idx; _ -> Nothing

        vecCount TF.Vec3 = 3
        vecCount TF.Vec4 = 4
        vecCount _ = error "Invalid accessor type for color attribute"
        colorComponents = map (\idx -> vecCount $ TF.accessorType $ preparedAccessor $ accessors V.! idx) colorIndices

        definitions =
             (if TF.Normal `HMS.member` primitiveAttributes then [("HAS_NORMALS", Nothing)] else [])
          ++ (if TF.Tangent `HMS.member` primitiveAttributes then [("HAS_TANGENTS", Nothing)] else [])
          ++ [ ("TEX_COORDS_COUNT", Just $ B.pack $ show texCoordsCount)
             , ("COLORS_COUNT", Just $ B.pack $ show colorsCount)
             , ("JOINTS_COUNT", Just $ B.pack $ show jointsCount)
             , ("WEIGHTS_COUNT", Just $ B.pack $ show weightsCount)
             ]
          ++ zipWith (\(idx :: Int) (count :: Int) -> ([i|COLOR_#{idx}_COMPONENTS|], Just $ B.pack $ show count)) [0..] colorComponents

type PipelinePair = (PipelineMeta, LoadedPipeline)

loadPrimitive :: MonadCube m => PipelineCache PipelineMeta -> Vector TF.Material -> Vector PreparedAccessor -> TF.Primitive -> StateT (IntMap PipelinePair) m (Maybe LoadedPrimitive)
loadPrimitive plCache materials accessors primitive@(TF.Primitive {..})
  | not (TF.Position `HMS.member` primitiveAttributes) = return Nothing
  | otherwise = do
      forM_ (HMS.toList primitiveAttributes) $ \(typ, accessorIndex) -> do
        let accessor = accessors V.! accessorIndex
        unless (TF.accessorIsValid typ $ preparedAccessor accessor) $ fail "Invalid accessor for this attribute type"

      let definitions = primitiveToDefinitions materials accessors primitive
      (meta, pl) <- getOrCompilePipeline definitions plCache
      modify $ IMS.insert (loadedPipelineId pl) (meta, pl)

      primitivesVAO <- newVAO
      (numIndices, sourceData) <-
            case primitiveIndices of
              Nothing ->
                let PreparedAccessor {..} = accessors V.! (primitiveAttributes HMS.! TF.Position)
                    src = Primitives { firstIndex = 0 }
                in return (TF.accessorCount preparedAccessor, src)
              Just indicesIndex -> do
                let PreparedAccessor {..} = accessors V.! indicesIndex
                unless (preparedStride == 0) $ fail "Strides for indice accessors are not supported"
                indicesType <-
                  case TF.accessorComponentType preparedAccessor of
                    TF.UnsignedByte -> return IWord8
                    TF.UnsignedShort -> return IWord16
                    TF.UnsignedInt -> return IWord32
                    invalidTyp -> fail $ [i|Invalid accessor #{indicesIndex} type for indices: #{invalidTyp}|]
                let src = PrimitivesWithIndices { indexBuffer = preparedBuffer
                                                , indexOffset = preparedOffset
                                                , indexType = indicesType
                                                }
                return (TF.accessorCount preparedAccessor, src)
      let TF.Material {} = maybe TF.defaultMaterial (materials V.!) primitiveMaterial
          lprimDrawCommand = drawCommand { primitiveType =
                                             case fromMaybe TF.Triangles primitiveMode of
                                               TF.Points -> Points
                                               TF.Lines -> Lines
                                               TF.LineLoop -> LineLoop
                                               TF.LineStrip -> LineStrip
                                               TF.Triangles -> Triangles
                                               TF.TriangleStrip -> TriangleStrip
                                               TF.TriangleFan -> TriangleFan
                                         , primitivesVAO
                                         , numIndices
                                         , sourceData
                                         }

      forM_ (HMS.toList $ pipelineAttributes meta) $ \(typ, plAttrIndex) -> do
        let accessorIndex = primitiveAttributes HMS.! typ
            PreparedAccessor { preparedAccessor = TF.Accessor {..}, ..} = accessors V.! accessorIndex
            sourcing = defaultSourcing { offset = preparedOffset
                                       , components = TF.accessorComponentsNumber accessorType
                                       , stride = preparedStride
                                       , normalize = fromMaybe False accessorNormalized
                                       , sourceType =
                                           case accessorComponentType of
                                             TF.Byte -> SInt8
                                             TF.UnsignedByte -> SWord8
                                             TF.Short -> SInt16
                                             TF.UnsignedShort -> SWord16
                                             TF.UnsignedInt -> SWord32
                                             TF.Float -> SFloat
                                       , attributeIndex = plAttrIndex
                                       , integerMapping = TF.attributeIsIntegral typ
                                       }
        sourceVertexData preparedBuffer sourcing primitivesVAO

      return $ Just LoadedPrimitive { lprimPipelineId = loadedPipelineId pl, ..}

loadMesh :: MonadCube m => PipelineCache PipelineMeta -> Vector TF.Material -> Vector PreparedAccessor -> TF.Mesh -> StateT (IntMap PipelinePair) m (Maybe LoadedMesh)
loadMesh plCache materials accessors (TF.Mesh {..}) = do
  lmeshPrimitives <- V.fromList <$> catMaybes <$> mapM (loadPrimitive plCache materials accessors) (V.toList meshPrimitives)
  if V.null lmeshPrimitives then
    return Nothing
  else
    return $ Just $ LoadedMesh {..}

data LoadedNodes = LoadedNodes { loadedNodes :: HashMap NodeName LoadedNodeTree
                               , loadedPipelines :: IntMap PipelinePair
                               }

instance Semigroup LoadedNodes where
  a <> b = LoadedNodes { loadedNodes = HMS.union (loadedNodes a) (loadedNodes b)
                       , loadedPipelines = IMS.union (loadedPipelines a) (loadedPipelines b)
                       }

instance Monoid LoadedNodes where
  mempty = LoadedNodes { loadedNodes = HMS.empty
                       , loadedPipelines = IMS.empty
                       }

data PipelineMeta = PipelineMeta { pipelineAttributes :: HashMap TF.AttributeType AttributeLocation
                                 , pipelineViewProjectionMatrix :: Maybe UniformLocation
                                 , pipelineModelMatrix :: Maybe UniformLocation
                                 , pipelineNormalMatrix :: Maybe UniformLocation
                                 }
                  deriving (Show, Eq)

prefixedVariable :: ByteString -> (Int -> TF.AttributeType) -> Atto.Parser TF.AttributeType
prefixedVariable prefix constr = do
  _ <- Atto.string prefix
  idx <- (Atto.char '_' *> Atto.decimal) <|> pure 0
  return $ constr idx

attributeVariableParser :: Atto.Parser TF.AttributeType
attributeVariableParser =
      (Atto.string "position" $> TF.Position)
  <|> (Atto.string "normal" $> TF.Normal)
  <|> (Atto.string "tangent" $> TF.Tangent)
  <|> prefixedVariable "texCoord" TF.TexCoord
  <|> prefixedVariable "color" TF.Color
  <|> prefixedVariable "joint" TF.Joints
  <|> prefixedVariable "weight" TF.Weights

getPipelineAttributes :: HashMap AttributeName (AttributeLocation, AttributeInfo) -> Either String (HashMap TF.AttributeType AttributeLocation)
getPipelineAttributes loadedAttributes = HMS.fromList <$> mapM mapAttribute (HMS.elems loadedAttributes)
  where mapAttribute (idx, AttributeInfo {..})
          | attributeSize /= 1 = Left [i|Array attributes are not supported: #{attributeName}|]
          | otherwise = do
              case Atto.parseOnly (attributeVariableParser <* Atto.endOfInput) attributeName of
                Left e -> Left [i|Couldn't parse attribute name #{attributeName}: #{e}|]
                Right r -> return (r, idx)

getPipelineUniforms :: HashMap UniformName (UniformLocation, UniformInfo) -> Either String [PipelineMeta -> PipelineMeta]
getPipelineUniforms loadedUniforms = mapM mapAttribute (HMS.elems loadedUniforms)
  where mapAttribute (idx, UniformInfo {..})
          | uniformSize /= 1 = Left "Array uniforms are not supported"
          | otherwise =
            case uniformName of
              "viewProjectionMatrix" -> return $ \x -> x { pipelineViewProjectionMatrix = Just idx }
              "modelMatrix" -> return $ \x -> x { pipelineModelMatrix = Just idx }
              "normalMatrix" -> return $ \x -> x { pipelineNormalMatrix = Just idx }
              _ -> Left [i|Unknown uniform #{uniformName}|]

getPipelineMeta :: LoadedPipeline -> Either String PipelineMeta
getPipelineMeta (LoadedPipeline {..}) = do
  plAttributes <- getPipelineAttributes loadedAttributes
  plUniformUpdates <- getPipelineUniforms loadedUniforms
  let initialMeta = PipelineMeta { pipelineAttributes = plAttributes
                                 , pipelineViewProjectionMatrix = Nothing
                                 , pipelineModelMatrix = Nothing
                                 , pipelineNormalMatrix = Nothing
                                 }
  return $ foldr ($) initialMeta plUniformUpdates

newCubePipelineCache :: MonadCube m => ShaderWithIncludes -> ShaderWithIncludes -> m (PipelineCache PipelineMeta)
newCubePipelineCache = newPipelineCache getPipelineMeta

loadNodes :: forall m. MonadCube m => PipelineCache PipelineMeta -> TF.BoundGlTF -> m LoadedNodes
loadNodes plCache (TF.BoundGlTF {..}) = do
  let neededBufferViews = S.fromList $ mapMaybe TF.accessorBufferView $ V.toList $ fromMaybe V.empty $ TF.gltfAccessors boundGltf
      runLoadBuffer idx = do
        loaded <- loadBuffer (boundBufferViews V.! idx)
        return (idx, loaded)
  bufferViews <- HMS.fromList <$> mapM runLoadBuffer (S.toList neededBufferViews)
  accessors <-
    case mapM (prepareAccessor bufferViews) $ fromMaybe V.empty $ TF.gltfAccessors boundGltf of
      Left e -> fail $ "Failed to validate accessors: " ++ e
      Right r -> return r
  let materials = fromMaybe V.empty $ TF.gltfMaterials boundGltf
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
  (nodes, loadedPipelines) <- flip runStateT IMS.empty $ mapM loadTopLevel $ V.toList treeNodes
  return LoadedNodes { loadedNodes = HMS.fromList nodes, ..}
