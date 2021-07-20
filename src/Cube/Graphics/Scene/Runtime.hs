-- | Scene loading and caching.

module Cube.Graphics.Scene.Runtime
  ( ModelId
  , SceneGraphModel(..)
  , SceneGraphNode(..)
  , SceneGraph
  , sgGraph
  , SceneOptions(..)
  , newSceneGraph
  , addScene
  ) where

import Data.Maybe
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
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.ShadersCache
import Cube.Graphics.Scene.Types
import Cube.Graphics.Scene.Resources
import Cube.Graphics.Model (loadModel, LoadedModel, CubePipelineCache)
import Data.WeakCache (WeakCache)
import qualified Data.WeakCache as WeakCache

type ModelId = Int

data SceneGraphModel = SceneGraphModel { sgmModel :: LoadedModel
                                       , sgmName :: ModelName
                                       , sgmId :: ModelId
                                       }

data SceneGraphNode = SceneGraphNode { sgnTrs :: TRSF
                                     , sgnModel :: Maybe SceneGraphModel
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

loadSceneModel :: MonadCube m => ModelName -> SceneT m SceneGraphModel
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
  WeakCache.getOrCreate name create (sgModelCache infoGraph)

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
