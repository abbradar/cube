-- | Scene loading and caching.

module Cube.Graphics.Assets
  ( SceneT
--  , SceneGraphModel(..)
--  , ModelInstance(..)
  , SceneGraphNode(..)
  , SceneInfo(..)
  , loadSceneNode
  , newSceneInfo
  , nodeTransform
--  , SceneGraph
--  , sgGraph
  , SceneOptions(..)
--  , newSceneGraph
--  , addScene
--  , addChunksToScene
--  , mapSceneM_
--  , mapSceneWithTRSM_
--  , advanceAnimations
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
import Cube.Map
import Data.WeakCache (WeakCache)
import qualified Data.WeakCache as WeakCache

--data SceneGraphModel = SceneGraphModel { sgmModel :: LoadedModel
--                                       , sgmName :: ModelName
--                                       , sgmId :: ModelId
--                                       } deriving Show


data SceneModel = SceneModel { smName :: ModelName, smModel :: LoadedModel }
  deriving (Show)

type SceneModelAnimation  =  (AnimationName, Maybe (IORef AnimationState))

--instance Show ComponentAnimations
--  where show (a,b) = show a

unzipMaybe :: Maybe (a, b) -> (Maybe a, Maybe b)
unzipMaybe Nothing = (Nothing, Nothing)
unzipMaybe (Just (a, b)) = (Just a, Just b)


--instance Show ComponentAnimation
--  where show _ = ""

--data ModelInstance = ModelInstance { instanceModel :: SceneGraphModel
--                                   , instanceAnimationRef :: Maybe (IORef AnimationState)
--                                   }

--instance Show ModelInstance
--  where show ModelInstance{..} = show instanceModel

data SceneGraphNode = SceneGraphNode { sgnTrs :: M44F
                                     , sgnModel :: Maybe SceneModel
                                     , sgnAnimation :: Maybe SceneModelAnimation
                                     , sgnChildren :: Vector SceneGraphNode
                                     }
instance Show SceneGraphNode
  where show SceneGraphNode{..} = show sgnTrs ++ show sgnModel ++ show sgnChildren

--data SceneGraph = SceneGraph { sgModelPaths :: HashMap ModelName FilePath
--                             , sgModelCache :: WeakCache ModelName SceneGraphModel
--                             , sgShaderCache :: CubePipelineCache
--                             , sgGraph :: Vector SceneGraphNode
--                             , sgLastModelId :: Int
--                             }
--instance Show SceneGraph
--  where show SceneGraph{..} = show sgGraph


data SceneOptions = SceneOptions { sceneVertexShader :: ShaderWithIncludes
                                 , sceneFragmentShader :: ShaderWithIncludes
                                 }

--newtype SceneState = SceneState { stateLastModelId :: Int
--                                }

data SceneInfo = SceneInfo { infoPreloadedModels :: HashMap ModelName TF.BoundGlTF
                           , infoModelPaths :: HashMap ModelName FilePath
                           , infoModelCache :: WeakCache ModelName SceneModel
                           , infoShaderCache :: CubePipelineCache
                           }

type SceneT m = StateT SceneInfo m

newSceneInfo :: MonadCube m => SceneOptions -> m SceneInfo
newSceneInfo (SceneOptions {..}) = do
  shaderCache <- newPipelineCache sceneVertexShader sceneFragmentShader
  modelCache <- WeakCache.new
  return $ SceneInfo { infoModelPaths = HM.empty
                      , infoModelCache = modelCache
                      , infoShaderCache = shaderCache
                      , infoPreloadedModels = HM.empty
                      }

nodeTransform :: SceneNode -> Either String M44F
nodeTransform (SceneNode { sceneNodeMatrix = Just mtx, sceneNodeRotation = Nothing, sceneNodeScale = Nothing, sceneNodeTranslation = Nothing }) = return mtx
nodeTransform (SceneNode { sceneNodeMatrix = Nothing, .. }) =
  let trs = TRS { trsTranslation = fromMaybe (V3 0 0 0) sceneNodeTranslation
                , trsRotation = fromMaybe (Quaternion 1 (V3 0 0 0)) sceneNodeRotation
                , trsScale = fromMaybe (V3 1 1 1) sceneNodeScale
                }
  in return $ trsToMatrix trs
nodeTransform _ = Left "Both transformation matrix and TRS values are specified"

loadSceneModel :: MonadCube m => ModelName -> SceneT m (SceneModel, SceneModelAnimation)
loadSceneModel name = do
  SceneInfo {..} <- get
  let create = do
        let model = infoPreloadedModels HM.! name
        loaded <- loadModel infoShaderCache model
        return $ SceneModel { smModel = loaded
                            , smName = name
                            }
  instanceModel <- WeakCache.getOrCreate name create infoModelCache
  instanceAnimationRef <-
    case HM.lookup "idle" $ loadedAnimations $ smModel instanceModel of
      Nothing -> return Nothing
      Just anim -> fmap Just $ liftIO $ newIORef $ startAnimation (defaultAnimationOptions { aoptsLoop = True }) Nothing anim
  return $ (SceneModel {..}, ("idle", instanceAnimationRef))

loadSceneNode :: MonadCube m => SceneNode -> SceneT m SceneGraphNode
loadSceneNode node@(SceneNode {..}) = do
  sgnTrs <- either fail return $ nodeTransform node
  modelAnimation <- mapM loadSceneModel sceneNodeModel
  sgnChildren <- mapM loadSceneNode $ fromMaybe V.empty sceneNodeChildren
  let (sgnModel, sgnAnimation) = unzipMaybe modelAnimation
  return $ SceneGraphNode {..}



checkSameModel :: FilePath -> FilePath -> FilePath
checkSameModel f1 f2
  | f1 == f2 = f2


addChunksToScene :: MonadCube m => Map -> BoundMap -> SceneT m SceneGraphNode
addChunksToScene mp (BoundMap {..}) = do
  SceneInfo{..} <- get
  model <- loadMapModel infoShaderCache mp bmapMaterials
  let node = SceneGraphNode{ sgnTrs = identity, sgnModel = Just SceneModel{ smModel = model, smName = "Map"}, sgnAnimation = Nothing, sgnChildren = V.empty }
  return $ node

--mapSceneM_ :: Monad m => (ModelInstance -> m ()) -> SceneGraph -> m ()
--mapSceneM_ f graph = mapM_ go $ V.toList $ sgGraph graph
--  where go (SceneGraphNode {..}) = do
--          mapM_ f sgnModel
--          mapM_ go $ V.toList sgnChildren

--mapSceneWithTRSM_ :: Monad m => (M44F -> ModelInstance -> m ()) -> SceneGraph -> m ()
--mapSceneWithTRSM_ f graph = mapM_ (go identity) $ V.toList $ sgGraph graph
--  where go parentTrs (SceneGraphNode {..}) = do
--          let trs = parentTrs !*! sgnTrs
--          mapM_ (f trs) sgnModel
--          mapM_ (go trs) $ V.toList sgnChildren

-- Animation should be advanced before each render.
--advanceAnimations :: MonadCube m => Timestamp -> SceneGraph -> m ()
--advanceAnimations currentTime = mapSceneM_ advance
--  where advance (ModelInstance { instanceAnimationRef = Nothing }) = return ()
--        advance (ModelInstance { instanceAnimationRef = Just ref }) = liftIO $ modifyIORef' ref (advanceAnimation currentTime)
