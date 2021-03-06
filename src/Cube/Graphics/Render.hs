-- | Render node tree.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Render
  ( PreparedMesh(..)
  , PreparedMaterialMeshes(..)
  , PreparedPipeline(..)
  , PreparedNodes
  , prepareSceneGraph
  , drawPreparedNodes
  , runDrawPreparedNodes
  ) where

import Data.IORef
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
import Cube.Graphics.Model
import Cube.Graphics.Screen
import Cube.Graphics.Camera
import Cube.Graphics.Animation
import Cube.Graphics.ShadersCache
import Cube.Graphics.Scene.Runtime

data PreparedMesh = PreparedMesh { preparedModelMatrix :: M44F
                                 , preparedDrawCommands :: [DrawCommand]
                                 }

instance Semigroup PreparedMesh where
  a <> b = PreparedMesh { preparedModelMatrix = preparedModelMatrix b
                        , preparedDrawCommands = preparedDrawCommands a ++ preparedDrawCommands b
                        }

data PreparedMaterialMeshes = PreparedMaterialMeshes { preparedTextures :: IntMap Texture
                                                     , preparedMaterial :: LoadedMaterial
                                                     , preparedFragmentPassTests :: FragmentPassTests
                                                     , preparedMeshes :: [PreparedMesh]
                                                     }

instance Semigroup PreparedMaterialMeshes where
  a <> b = PreparedMaterialMeshes { preparedTextures = preparedTextures b
                                  , preparedMaterial = preparedMaterial b
                                  , preparedFragmentPassTests = preparedFragmentPassTests b
                                  , preparedMeshes = preparedMeshes a ++ preparedMeshes b
                                  }

data PreparedPipeline = PreparedPipeline { preparedPipeline :: LoadedPipeline
                                         , preparedMeta :: PipelineMeta
                                         , preparedMaterialMeshes :: HashMap (ModelId, MaterialId) PreparedMaterialMeshes
                                         }

instance Semigroup PreparedPipeline where
  a <> b = PreparedPipeline { preparedPipeline = preparedPipeline b
                            , preparedMeta = preparedMeta b
                            , preparedMaterialMeshes = HM.unionWith (<>) (preparedMaterialMeshes a) (preparedMaterialMeshes b)
                            }

data HalfPreparedMaterialMeshes = HalfPreparedMaterialMeshes { halfPreparedMaterial :: LoadedMaterial
                                                             , halfPreparedCommands :: [DrawCommand]
                                                             }

instance Semigroup HalfPreparedMaterialMeshes where
  a <> b = HalfPreparedMaterialMeshes { halfPreparedMaterial = halfPreparedMaterial b
                                      , halfPreparedCommands = halfPreparedCommands a ++ halfPreparedCommands b
                                      }

data HalfPreparedPipeline = HalfPreparedPipeline { halfPreparedPipeline :: LoadedPipeline
                                                 , halfPreparedMeta :: PipelineMeta
                                                 , halfPreparedMaterialMeshes :: HashMap (ModelId, MaterialId) HalfPreparedMaterialMeshes
                                                 }

instance Semigroup HalfPreparedPipeline where
  a <> b = HalfPreparedPipeline { halfPreparedPipeline = halfPreparedPipeline b
                                , halfPreparedMeta = halfPreparedMeta b
                                , halfPreparedMaterialMeshes = HM.unionWith (<>) (halfPreparedMaterialMeshes a) (halfPreparedMaterialMeshes b)
                                }

type PreparedNodes = IntMap PreparedPipeline

prepareSceneGraphModel :: forall m. MonadCube m => TRSF -> ModelInstance -> StateT PreparedNodes m ()
prepareSceneGraphModel initialTrs (ModelInstance { instanceModel = SceneGraphModel {..}, .. }) = do
  animationNodes <-
    case instanceAnimationRef of
      Nothing -> return IM.empty
      Just animRef -> fmap (lanimNodes . astateAnimation) $ liftIO $ readIORef animRef

  let go :: TRSF -> LoadedNodeTree -> StateT PreparedNodes m ()
      go parentTrs (LoadedNodeTree {..}) = do
        let animTrs = maybe mempty groupAnimationMorph $ IM.lookup lnodeIndex animationNodes
            trs = parentTrs <> lnodeTrs <> animTrs
            trsMatrix = trsToMatrix trs
        case lnodeMesh of
          Nothing -> return ()
          Just (LoadedMesh {..}) -> do
            let halfMapPrimitive (LoadedPrimitive {..}) = (pipelineId lprimPipelineMeta, preparedPl)
                  where LoadedMaterial {..} = lprimMaterial
                        preparedPl = HalfPreparedPipeline { halfPreparedPipeline = lprimPipeline
                                                          , halfPreparedMeta = lprimPipelineMeta
                                                          , halfPreparedMaterialMeshes = HM.singleton (sgmId, lmatId) preparedMeshes
                                                          }
                        preparedMeshes = HalfPreparedMaterialMeshes { halfPreparedMaterial = lprimMaterial
                                                                    , halfPreparedCommands = [lprimDrawCommand]
                                                                    }

                finalizePipeline (HalfPreparedPipeline {..}) =
                  PreparedPipeline { preparedPipeline = halfPreparedPipeline
                                   , preparedMeta = halfPreparedMeta
                                   , preparedMaterialMeshes = HM.map finalizeMaterialMeshes halfPreparedMaterialMeshes
                                   }

                finalizeMaterialMeshes (HalfPreparedMaterialMeshes {..}) =
                  PreparedMaterialMeshes { preparedTextures = IM.fromList $ map (\(typ, (_subIdx, tex)) -> (fromEnum typ, tex)) $ HM.toList lmatTextures
                                         , preparedMaterial = halfPreparedMaterial
                                         , preparedFragmentPassTests = defaultFragmentPassTests { cullFace = if lmatDoubleSided then NoCulling else Back
                                                                                                , writeDepth = True
                                                                                                , depthTest = Just Less
                                                                                                }
                                         , preparedMeshes = [preparedMesh]
                                         }

                  where LoadedMaterial {..} = halfPreparedMaterial
                        preparedMesh = PreparedMesh { preparedModelMatrix = trsMatrix
                                                    , preparedDrawCommands = halfPreparedCommands
                                                    }

            let prepared = IM.map finalizePipeline $ IM.fromListWith (<>) $ map halfMapPrimitive $ V.toList lmeshPrimitives
            modify' $ IM.unionWith (<>) prepared

        mapM_ (go trs) lnodeChildren

  mapM_ (go initialTrs) $ loadedNodes sgmModel

prepareSceneGraph :: forall m. MonadCube m => SceneGraph -> m PreparedNodes
prepareSceneGraph sg = flip execStateT IM.empty $ mapSceneWithTRSM_ prepareSceneGraphModel sg

drawPreparedNodesGeneric :: MonadCube m => Bool -> ScreenF -> CameraF -> PreparedNodes -> DrawT m ()
drawPreparedNodesGeneric setFirstPipeline (Screen {..}) camera = foldM_ drawPipeline setFirstPipeline
  where viewMatrix = cameraToMatrix camera
        viewProjectionMatrix = projectionMatrix !*! viewMatrix
        drawPipeline doSetPipeline (PreparedPipeline {..}) = do
          when doSetPipeline $ setPipeline $ loadedPipeline preparedPipeline

          let setPipelineUniform :: (MonadIO m, Uniformable a) => (PipelineMeta -> Maybe UniformLocation) -> a -> m ()
              setPipelineUniform accessor value =
                case accessor preparedMeta of
                  Nothing -> return ()
                  Just idx -> setUniform value idx $ loadedPipeline preparedPipeline

          setPipelineUniform pipelineViewProjectionMatrix $ transpose viewProjectionMatrix
          setPipelineUniform pipelineCamera $ cameraPosition camera

          forM_ (HM.toList $ pipelineTextures preparedMeta) $ \(texType, idx) ->
            setUniform (fromEnum texType) idx $ loadedPipeline preparedPipeline

          forM_ preparedMaterialMeshes $ \PreparedMaterialMeshes {..} -> do
            setTextureBindings preparedTextures
            setPipelineUniform pipelineBaseColorFactor $ lmatBaseColorFactor preparedMaterial
            setPipelineUniform pipelineMetallicFactor $ lmatMetallicFactor preparedMaterial
            setPipelineUniform pipelineRoughnessFactor $ lmatRoughnessFactor preparedMaterial
            setFragmentPassTests preparedFragmentPassTests

            forM_ preparedMeshes $ \PreparedMesh {..} -> do
              setPipelineUniform pipelineModelMatrix $ transpose preparedModelMatrix
              setPipelineUniform pipelineNormalMatrix $ transpose $ inv44 (viewMatrix !*! preparedModelMatrix)
              mapM_ drawR preparedDrawCommands

          return True

drawPreparedNodes :: MonadCube m => ScreenF -> CameraF -> PreparedNodes -> DrawT m ()
drawPreparedNodes = drawPreparedNodesGeneric True

runDrawPreparedNodes :: MonadCube m => DrawParams -> ScreenF -> CameraF -> PreparedNodes -> m ()
runDrawPreparedNodes params screen camera nodes
  | IM.null nodes = return ()
  | otherwise = runDraws newParams $ drawPreparedNodesGeneric False screen camera nodes
  where (firstPl:_) = IM.elems nodes
        newParams = params { pipeline = loadedPipeline $ preparedPipeline firstPl }
