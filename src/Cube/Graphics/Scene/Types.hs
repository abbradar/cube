-- | Scene implementation.

module Cube.Graphics.Scene.Types
  ( ModelName
  , SceneNode(..)
  , Scene(..)
  ) where

import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Vector (Vector)
import Data.HashMap.Strict (HashMap)
import Data.Aeson

import Cube.Graphics.Types
import Data.Aeson.Utils
import Linear.Aeson ()

sceneOptions :: String -> Options
sceneOptions prefix = defaultOptions { omitNothingFields = False
                                     , fieldLabelModifier = removePrefix prefix
                                     }

type ModelName = Text

data SceneNode = SceneNode { sceneNodeTranslation :: Maybe V3F
                           , sceneNodeRotation :: Maybe QF
                           , sceneNodeScale :: Maybe V3F
                           , sceneNodeModel :: Maybe ModelName
                           , sceneNodeMatrix :: Maybe M44F
                           , sceneNodeChildren :: Maybe (Vector SceneNode)
                           }
               deriving (Show, Generic)

instance FromJSON SceneNode where
  parseJSON = genericParseJSON $ sceneOptions "sceneNode"

data Scene = Scene { sceneModels :: Maybe (HashMap ModelName FilePath)
                   , sceneGraph :: Maybe (Vector SceneNode)
                   }
           deriving (Show, Generic)

instance FromJSON Scene where
  parseJSON = genericParseJSON $ sceneOptions "scene"
