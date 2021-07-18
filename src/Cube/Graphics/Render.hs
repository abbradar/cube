-- | Render node tree.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Render
  ( PreparedMesh(..)
  , PreparedMaterialMeshes(..)
  , PreparedPipeline(..)
  , prepareLoadedNodes
  , drawPreparedPipelines
  , runDrawPreparedPipelines
  ) where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V
import Control.Monad.State.Strict
import Graphics.Caramia
import Linear

import Cube.Types
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.Resources
import Cube.Graphics.Screen
import Cube.Graphics.Camera
import Cube.Graphics.ShadersCache

data PreparedMesh = PreparedMesh { preparedModelMatrix :: MF44
                                 , preparedDrawCommands :: [DrawCommand]
                                 }

data PreparedMaterialMeshes = PreparedMaterialMeshes { preparedTextures :: IntMap Texture
                                                     , preparedBaseColorFactor :: V4 Float
                                                     , preparedMetallicFactor :: Float
                                                     , preparedRoughnessFactor :: Float
                                                     , preparedMeshes :: [PreparedMesh]
                                                     }

instance Semigroup PreparedMaterialMeshes where
  a <> b = PreparedMaterialMeshes { preparedTextures = preparedTextures b
                                  , preparedBaseColorFactor = preparedBaseColorFactor b
                                  , preparedMetallicFactor = preparedMetallicFactor b
                                  , preparedRoughnessFactor = preparedRoughnessFactor b
                                  , preparedMeshes = preparedMeshes a ++ preparedMeshes b
                                  }

data PreparedPipeline = PreparedPipeline { preparedPipeline :: Pipeline
                                         , preparedMeta :: PipelineMeta
                                         , preparedMaterialMeshes :: HashMap (Maybe MaterialId) PreparedMaterialMeshes
                                         }

instance Semigroup PreparedPipeline where
  a <> b = PreparedPipeline { preparedPipeline = preparedPipeline b
                            , preparedMeta = preparedMeta b
                            , preparedMaterialMeshes = HM.unionWith (<>) (preparedMaterialMeshes a) (preparedMaterialMeshes b)
                            }

type PreparedNodes = IntMap PreparedPipeline

prepareLoadedNodes :: LoadedNodes -> PreparedNodes
prepareLoadedNodes nodes = flip execState IM.empty $ mapM_ (go mempty) $ loadedNodes nodes
  where go :: TRSF -> LoadedNodeTree -> State (IntMap PreparedPipeline) ()
        go parentTrs (LoadedNodeTree {..}) = do
          let trs = parentTrs <> lnodeTrs
          case lnodeMesh of
            Nothing -> return ()
            Just (LoadedMesh {..}) -> do
              let mapPrimitive prim = (lprimPipelineId prim, HM.singleton (lprimMaterialId prim) [lprimDrawCommand prim])
                  pipelineMeshes = IM.fromListWith (HM.unionWith (++)) $ map mapPrimitive $ V.toList lmeshPrimitives

                  makeMaterialMesh :: Maybe MaterialId -> [DrawCommand] -> PreparedMaterialMeshes
                  makeMaterialMesh matId drawCommands =
                    PreparedMaterialMeshes { preparedTextures = IM.fromList $ map (\(typ, (_subIdx, tex)) -> (fromEnum typ, tex)) $ HM.toList lmatTextures
                                           , preparedBaseColorFactor = lmatBaseColorFactor
                                           , preparedMetallicFactor = lmatMetallicFactor
                                           , preparedRoughnessFactor = lmatRoughnessFactor
                                           , preparedMeshes = [PreparedMesh { preparedModelMatrix = trsToMatrix trs
                                                                            , preparedDrawCommands = drawCommands
                                                                            }]
                                           }
                            where LoadedMaterial {..} =
                                    case matId of
                                      Nothing -> defaultMaterial
                                      Just i -> loadedMaterials nodes IM.! i

                  addPipeline :: IntMap PreparedPipeline -> Int -> HashMap (Maybe MaterialId) [DrawCommand] -> IntMap PreparedPipeline
                  addPipeline pls plId meshCommands = IM.alter modifyPipeline plId pls
                    where
                      meshes = HM.mapWithKey makeMaterialMesh meshCommands
                      modifyPipeline (Just oldPl) = Just oldPl { preparedMaterialMeshes = HM.unionWith (<>) meshes (preparedMaterialMeshes oldPl)
                                                               }
                      modifyPipeline Nothing = Just PreparedPipeline { preparedPipeline = loadedPipeline pl
                                                                     , preparedMeta = meta
                                                                     , preparedMaterialMeshes = meshes
                                                                     }
                        where (meta, pl) = loadedPipelines nodes IM.! plId

              modify' $ \pls -> IM.foldlWithKey' addPipeline pls pipelineMeshes

          mapM_ (go trs) lnodeChildren

drawPreparedPipelinesGeneric :: (MonadCube m) => Bool -> ScreenF -> CameraF -> PreparedNodes -> DrawT m ()
drawPreparedPipelinesGeneric setFirstPipeline (Screen {..}) camera = foldM_ drawPipeline setFirstPipeline
  where viewMatrix = cameraToMatrix camera
        viewProjectionMatrix = projectionMatrix !*! viewMatrix
        drawPipeline doSetPipeline (PreparedPipeline {..}) = do
          when doSetPipeline $ setPipeline preparedPipeline

          let setPipelineUniform :: (MonadIO m, Uniformable a) => (PipelineMeta -> Maybe UniformLocation) -> a -> m ()
              setPipelineUniform accessor value =
                case accessor preparedMeta of
                  Nothing -> return ()
                  Just idx -> setUniform value idx preparedPipeline

          setPipelineUniform pipelineViewProjectionMatrix $ transpose viewProjectionMatrix

          forM_ (HM.toList $ pipelineTextures preparedMeta) $ \(texType, idx) ->
            setUniform (fromEnum texType) idx preparedPipeline

          forM_ preparedMaterialMeshes $ \PreparedMaterialMeshes {..} -> do
            setTextureBindings preparedTextures
            setPipelineUniform pipelineBaseColorFactor preparedBaseColorFactor
            setPipelineUniform pipelineMetallicFactor preparedMetallicFactor
            setPipelineUniform pipelineRoughnessFactor  preparedRoughnessFactor

            forM_ preparedMeshes $ \PreparedMesh {..} -> do
              setPipelineUniform pipelineModelMatrix $ transpose preparedModelMatrix
              setPipelineUniform pipelineNormalMatrix $ transpose $ inv44 (viewMatrix !*! preparedModelMatrix)
              mapM_ drawR preparedDrawCommands

          return False

drawPreparedPipelines :: (MonadCube m) => ScreenF -> CameraF -> PreparedNodes -> DrawT m ()
drawPreparedPipelines = drawPreparedPipelinesGeneric True

runDrawPreparedPipelines :: (MonadCube m) => DrawParams -> ScreenF -> CameraF -> PreparedNodes -> m ()
runDrawPreparedPipelines params screen camera nodes
  | IM.null nodes = return ()
  | otherwise = runDraws newParams $ drawPreparedPipelinesGeneric False screen camera nodes
  where (firstPl:_) = IM.elems nodes
        newParams = params { pipeline = preparedPipeline firstPl }
