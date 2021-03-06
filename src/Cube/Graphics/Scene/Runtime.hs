-- | Scene loading and caching.

module Cube.Graphics.Scene.Runtime
  ( ModelId
  , SceneGraphModel(..)
  , ModelInstance(..)
  , SceneGraphNode(..)
  , SceneGraph
  , sgGraph
  , SceneOptions(..)
  , newSceneGraph
  , addScene
  , mapSceneM_
  , mapSceneWithTRSM_
  , advanceAnimations
  ) where

import Data.Maybe
import Data.IORef
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Control.Monad.Reader
import Control.Monad.State
import Linear

import qualified Data.GlTF.Resources as TF
import Data.GLSL.Preprocessor (ShaderWithIncludes)
import Cube.Types
import Cube.Time
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.Animation
import Cube.Graphics.ShadersCache
import Cube.Graphics.Scene.Types
import Cube.Graphics.Scene.Resources
import Cube.Graphics.Model
import Data.WeakCache (WeakCache)
import qualified Data.WeakCache as WeakCache

type ModelId = Int

data SceneGraphModel = SceneGraphModel { sgmModel :: LoadedModel
                                       , sgmName :: ModelName
                                       , sgmId :: ModelId
                                       }

data ModelInstance = ModelInstance { instanceModel :: SceneGraphModel
                                   , instanceAnimationRef :: Maybe (IORef AnimationState)
                                   }

data SceneGraphNode = SceneGraphNode { sgnTrs :: TRSF
                                     , sgnModel :: Maybe ModelInstance
                                     , sgnChildren :: Vector SceneGraphNode
                                     }

data SceneGraph = SceneGraph { sgModelPaths :: HashMap ModelName FilePath
                             , sgModelCache :: WeakCache ModelName SceneGraphModel
                             , sgShaderCache :: CubePipelineCache
                             , sgGraph :: Vector SceneGraphNode
                             , sgLastModelId :: Int
                             }

data SceneOptions = SceneOptions { sceneVertexShader :: ShaderWithIncludes
                                 , sceneFragmentShader :: ShaderWithIncludes
                                 }

newtype SceneState = SceneState { stateLastModelId :: Int
                                }

data SceneInfo = SceneInfo { infoPreloadedModels :: HashMap ModelName TF.BoundGlTF
                           , infoGraph :: SceneGraph
                           }

type SceneT m = StateT SceneState (ReaderT SceneInfo m)

newSceneGraph :: MonadCube m => SceneOptions -> m SceneGraph
newSceneGraph (SceneOptions {..}) = do
  shaderCache <- newPipelineCache sceneVertexShader sceneFragmentShader
  modelCache <- WeakCache.new
  return $ SceneGraph { sgModelPaths = HM.empty
                      , sgModelCache = modelCache
                      , sgShaderCache = shaderCache
                      , sgGraph = V.empty
                      , sgLastModelId = 0
                      }

nodeTransform :: SceneNode -> Either String TRSF
nodeTransform (SceneNode { sceneNodeMatrix = Just mtx, sceneNodeRotation = Nothing, sceneNodeScale = Nothing, sceneNodeTranslation = Nothing }) = return $ matrixToTRS mtx
nodeTransform (SceneNode { sceneNodeMatrix = Nothing, .. }) =
  return $ TRS { trsTranslation = fromMaybe (V3 0 0 0) sceneNodeTranslation
               , trsRotation = fromMaybe (Quaternion 1 (V3 0 0 0)) sceneNodeRotation
               , trsScale = fromMaybe (V3 1 1 1) sceneNodeScale
               }
nodeTransform _ = Left "Both transformation matrix and TRS values are specified"

loadSceneModel :: MonadCube m => ModelName -> SceneT m ModelInstance
loadSceneModel name = do
  SceneInfo {..} <- ask
  let create = do
        let model = infoPreloadedModels HM.! name
        myId <- gets stateLastModelId
        modify $ \x -> x { stateLastModelId = myId + 1 }
        loaded <- loadModel (sgShaderCache infoGraph) model
        return $ SceneGraphModel { sgmModel = loaded
                                 , sgmName = name
                                 , sgmId = myId
                                 }
  instanceModel <- WeakCache.getOrCreate name create (sgModelCache infoGraph)
  instanceAnimationRef <-
    case HM.lookup "idle" $ loadedAnimations $ sgmModel instanceModel of
      Nothing -> return Nothing
      Just anim -> fmap Just $ liftIO $ newIORef $ startAnimation (defaultAnimationOptions { aoptsLoop = True }) Nothing anim
  return $ ModelInstance {..}

loadSceneNode :: MonadCube m => SceneNode -> SceneT m SceneGraphNode
loadSceneNode node@(SceneNode {..}) = do
  sgnTrs <- either fail return $ nodeTransform node
  sgnModel <- mapM loadSceneModel sceneNodeModel
  sgnChildren <- mapM loadSceneNode $ fromMaybe V.empty sceneNodeChildren
  return $ SceneGraphNode {..}

checkSameModel :: FilePath -> FilePath -> FilePath
checkSameModel f1 f2
  | f1 == f2 = f2
  | otherwise = error "Different paths for the same model"

addScene :: MonadCube m => SceneGraph -> BoundScene -> m SceneGraph
addScene sg (BoundScene {..}) = do
  let sg' = sg { sgModelPaths = HM.unionWith checkSameModel (sgModelPaths sg) bsceneModelPaths
               }
      info = SceneInfo { infoPreloadedModels = bscenePreloadedModels
                       , infoGraph = sg'
                       }
      initial = SceneState { stateLastModelId = sgLastModelId sg' }
  (graph, state') <- flip runReaderT info $ flip runStateT initial $ mapM loadSceneNode bsceneGraph
  return $ sg' { sgGraph = sgGraph sg' <> graph
               , sgLastModelId = stateLastModelId state'
               }

mapSceneM_ :: Monad m => (ModelInstance -> m ()) -> SceneGraph -> m ()
mapSceneM_ f graph = mapM_ go $ V.toList $ sgGraph graph
  where go (SceneGraphNode {..}) = do
          mapM_ f sgnModel
          mapM_ go $ V.toList sgnChildren

mapSceneWithTRSM_ :: Monad m => (TRSF -> ModelInstance -> m ()) -> SceneGraph -> m ()
mapSceneWithTRSM_ f graph = mapM_ (go mempty) $ V.toList $ sgGraph graph
  where go parentTrs (SceneGraphNode {..}) = do
          let trs = parentTrs <> sgnTrs
          mapM_ (f trs) sgnModel
          mapM_ (go trs) $ V.toList sgnChildren

-- Animation should be advanced before each render.
advanceAnimations :: MonadCube m => Timestamp -> SceneGraph -> m ()
advanceAnimations currentTime = mapSceneM_ advance
  where advance (ModelInstance { instanceAnimationRef = Nothing }) = return ()
        advance (ModelInstance { instanceAnimationRef = Just ref }) = liftIO $ modifyIORef' ref (advanceAnimation currentTime)
