-- | High level game mechanics.

module Cube.Game ( GameSettings(..)
                 , GameWindow(..)
                 , GameState(..)
                 , GameInitialState(..)
                 , updatePlayer
) where

import GHC.Generics (Generic)

import qualified Data.HashSet as HS

import SDL hiding (Event)

import Cube.Graphics.Camera
import Cube.Graphics.TRS
import Cube.Graphics.Types
import Cube.Graphics.Scene.Runtime
import Cube.ECS
import Cube.Map

data GameSettings = GameSettings { gameVertexShader :: FilePath
                                 , gameFragmentShader :: FilePath
                                 , gameScene :: FilePath
                                 , gameMap :: FilePath
                                 , gameInitialPosition :: V3 Float
                                 , gameInitialRotation :: Quaternion Float
                                 , gameCameraShift :: V3 Float
                                 , gameInitialWidth :: Word
                                 , gameInitialHeight :: Word
                                 , gameNearPlane :: Float
                                 , gameFarPlane :: Float
                                 , gameFov :: Float
                                 , gameFrameRate :: Word
                                 , gameWorldRate :: Word
                                 }
                  deriving (Show, Eq, Generic)

-- Immutable values for the whole duration of running.
data GameWindow = GameWindow { gameWindow :: SDL.Window
                             , gameFovRadians :: Float
                             , gameSettings :: GameSettings
                             }

data GameState = GameState { stateScreen :: ScreenF
                           , stateScene :: SceneGraph
                           , stateWorld :: World
                           , stateMap :: Map
                           }

data GameInitialState = GameInitialState { initialScene :: SceneGraph
                                         , initialWorld :: World
                                         , initialMap :: Map
                                         }

updatePlayer :: Applicative f => (Maybe (V3 Float), Maybe (V2 Float)) -> World -> f World
updatePlayer (mmove, mrotation) wrld = do
  let
    movePlayer :: V3 Float -> Component -> Component
    movePlayer step (CTransform trs) = CTransform trs{ trsTranslation = trsTranslation trs + (0.1 *^ rotate (trsRotation trs) step) }
    movePlayer _ _ = error "wrong player component identifier"

    moveCamera :: V3 Float -> Component -> Component
    moveCamera step (CCamera cam) = CCamera cam{ fCameraTarget = (fCameraTarget cam) + 0.1 *^ rotate (fCameraRotation cam) step }
    moveCamera _ _ = error "wrong camera component identifier"

    rotateCamera :: V2 Float -> Component -> Component
    rotateCamera shift (CCamera cam) = CCamera $ fCameraRotateNoRoll shift cam
    rotateCamera _ _ = error "wrong camera component identifier"

    wrld' =
      case mmove of
        Nothing -> wrld
        Just step -> updateComponentByType (HS.fromList [2]) 2 (moveCamera step) $ updateComponentByType (HS.fromList [0,3]) 0 (movePlayer step) wrld--moveRotate pl (0.1 *^ rotate playerRotation step) (Quaternion 1.0 (V3 0.0 0.0 0.0)) initialMap
    wrld'' =
      case mrotation of
        Nothing -> wrld'
        Just shift -> updateComponentByType (HS.fromList [2]) 2 (rotateCamera shift) wrld'
  return wrld''
