-- | Scene loading and caching.

module Cube.Graphics.Scene.Resources
  ( BoundScene(..)
  , BoundMap(..)
  , readSceneFiles
  , readMapFiles
  ) where

import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Aeson as JSON
import Control.Concurrent.Async
import Control.Monad.Reader
import Control.Monad.State.Strict
import System.Directory
import System.FilePath

import qualified Data.GlTF.Resources as TF
import Cube.Types
import Cube.Graphics.Scene.Types
import Cube.Map

data BoundScene = BoundScene { bsceneModelPaths :: HashMap ModelName FilePath
                             , bscenePreloadedModels :: HashMap ModelName TF.BoundGlTF
                             , bsceneGraph :: Vector SceneNode
                             }

data BoundMap = BoundMap { bmapMaterials :: TF.BoundGlTF }

newtype LoadState = LoadState { currentPromises :: HashMap ModelName (Async TF.BoundGlTF)
                              }

newtype LoadInfo = LoadInfo { infoModelPaths :: HashMap ModelName FilePath
                            }

type SceneT m = StateT LoadState (ReaderT LoadInfo m)

readSceneNode :: MonadCube m => SceneNode -> SceneT m ()
readSceneNode (SceneNode {..}) = do
  case sceneNodeModel of
    Nothing -> return ()
    Just name -> do
      st <- get
      case name `HM.lookup` currentPromises st of
        Just _existing -> return ()
        Nothing -> do
          modelPaths <- asks infoModelPaths
          modelPromise <- liftIO $ async $ TF.readModel (modelPaths HM.! name)
          modify $ \x -> x { currentPromises = HM.insert name modelPromise $ currentPromises x }
  mapM_ readSceneNode $ fromMaybe V.empty sceneNodeChildren

readMapFiles :: MonadCube m => FilePath -> m (MapRandom, BoundMap)
readMapFiles mapPath = do
  MapData {..} <- liftIO (JSON.eitherDecodeFileStrict' mapPath) >>= either fail return
  materialPromise <- liftIO $ async $ TF.readModel mapPath
  preloadedMaterials <- liftIO $ wait $ materialPromise
  return (mapRandom, BoundMap { bmapMaterials = preloadedMaterials })


readSceneFiles :: MonadCube m => FilePath -> Vector SceneNode -> m BoundScene
readSceneFiles scenePath sceneNodes = do
  SceneModels {..} <- liftIO (JSON.eitherDecodeFileStrict' scenePath) >>= either fail return
  let basePath = takeDirectory scenePath
      -- Canonicalizing them is important, because we make sure model files are the same when merging the scene graph later.
  bsceneModelPaths <- liftIO $ mapM (canonicalizePath . (basePath </>)) $ fromMaybe HM.empty sceneModels
  let info = LoadInfo { infoModelPaths = bsceneModelPaths
                      }
      initial = LoadState { currentPromises = HM.empty
                          }

  let bsceneGraph = sceneNodes
  state' <- flip runReaderT info $ flip execStateT initial $ mapM_ readSceneNode bsceneGraph
  bscenePreloadedModels <- liftIO $ mapM wait $ currentPromises state'
  return $ BoundScene {..}
