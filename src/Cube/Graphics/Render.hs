-- | Render node tree.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Render
  ( PreparedMesh(..)
  , PreparedPipeline(..)
  , prepareLoadedNodes
  , drawPreparedPipelines
  , runDrawPreparedPipelines
  ) where

import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
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

data PreparedPipeline = PreparedPipeline { preparedPipeline :: Pipeline
                                         , preparedMeta :: PipelineMeta
                                         , preparedMeshes :: [PreparedMesh]
                                         }

prepareLoadedNodes :: LoadedNodes -> [PreparedPipeline]
prepareLoadedNodes nodes = IM.elems $ flip execState IM.empty $ mapM_ (go mempty) $ loadedNodes nodes
  where go :: TRSF -> LoadedNodeTree -> State (IntMap PreparedPipeline) ()
        go parentTrs (LoadedNodeTree {..}) = do
          let trs = parentTrs <> lnodeTrs
          case lnodeMesh of
            Nothing -> return ()
            Just (LoadedMesh {..}) -> do
              let pipelineMeshes = IM.fromListWith (++) $ map (\prim -> (lprimPipelineId prim, [lprimDrawCommand prim])) $ V.toList lmeshPrimitives

                  preparedModelMatrix = trsToMatrix trs

                  addPipeline :: IntMap PreparedPipeline -> Int -> [DrawCommand] -> IntMap PreparedPipeline
                  addPipeline pls plId preparedDrawCommands = IM.alter modifyPipeline plId pls
                    where mesh = PreparedMesh {..}
                          modifyPipeline (Just oldPl) = Just oldPl { preparedMeshes = mesh : preparedMeshes oldPl
                                                                   }
                          modifyPipeline Nothing = Just PreparedPipeline { preparedPipeline = loadedPipeline pl
                                                                         , preparedMeta = meta
                                                                         , preparedMeshes = [mesh]
                                                                         }
                            where (meta, pl) = loadedPipelines nodes IM.! plId

              modify' $ \pls -> IM.foldlWithKey' addPipeline pls pipelineMeshes

          mapM_ (go trs) lnodeChildren

drawPreparedPipelinesGeneric :: (MonadCube m) => Bool -> ScreenF -> CameraF -> [PreparedPipeline] -> DrawT m ()
drawPreparedPipelinesGeneric setFirstPipeline (Screen {..}) camera = foldM_ drawPipeline setFirstPipeline
  where viewMatrix = cameraToMatrix camera
        viewProjectionMatrix = projectionMatrix !*! viewMatrix
        drawPipeline doSetPipeline (PreparedPipeline {..}) = do
          when doSetPipeline $ setPipeline preparedPipeline
          case pipelineViewProjectionMatrix preparedMeta of
            Nothing -> return ()
            Just idx -> setUniform viewProjectionMatrix idx preparedPipeline

          forM_ preparedMeshes $ \(PreparedMesh {..}) -> do
            let normalMatrix = transpose $ inv44 (viewMatrix !*! preparedModelMatrix)
            case pipelineModelMatrix preparedMeta of
              Nothing -> return ()
              Just idx -> setUniform preparedModelMatrix idx preparedPipeline
            case pipelineNormalMatrix preparedMeta of
              Nothing -> return ()
              Just idx -> setUniform normalMatrix idx preparedPipeline
            mapM_ drawR preparedDrawCommands

          return False

drawPreparedPipelines :: (MonadCube m) => ScreenF -> CameraF -> [PreparedPipeline] -> DrawT m ()
drawPreparedPipelines = drawPreparedPipelinesGeneric True

runDrawPreparedPipelines :: (MonadCube m) => DrawParams -> ScreenF -> CameraF -> [PreparedPipeline] -> m ()
runDrawPreparedPipelines _ _ _ [] = return ()
runDrawPreparedPipelines params screen camera pls@(firstPl:_) = runDraws newParams $ drawPreparedPipelinesGeneric False screen camera pls
  where newParams = params { pipeline = preparedPipeline firstPl }
