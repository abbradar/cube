{-# LANGUAGE StrictData #-}

import GHC.Generics (Generic)
import System.Environment
import Data.Aeson as JSON
import Control.Concurrent.Async
import Control.Monad.IO.Class
import Control.Monad.Logger
import Graphics.Caramia as Caramia
import System.FilePath
import Reflex
import SDL hiding (Event)

import Data.Aeson.Utils
import qualified Data.GlTF.Resources as TF
import Data.GLSL.Preprocessor
import Cube.Types
import Cube.Loop.Stable
import Cube.Loop.Reflex
import Cube.Graphics.Types
import Cube.Graphics.Screen
import Cube.Graphics.Resources
import Cube.Graphics.Render

data GameSettings = GameSettings { gameVertexShader :: FilePath
                                 , gameFragmentShader :: FilePath
                                 , gameModels :: [FilePath]
                                 , gameInitialWidth :: Word
                                 , gameInitialHeight :: Word
                                 , gameNearPlane :: Float
                                 , gameFarPlane :: Float
                                 , gameFov :: Float
                                 , gameFrameRate :: Word
                                 , gameWorldRate :: Word
                                 }
                  deriving (Show, Eq, Generic)

instance FromJSON GameSettings where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = removePrefix "game"
                                                }

-- Immutable values for the whole duration of running.
data GameWindow = GameWindow { gameWindow :: SDL.Window
                             , gameFovRadians :: Float
                             }

data GameState = GameState { stateCamera :: CameraF
                           , stateScreen :: ScreenF
                           , stateNodes :: LoadedNodes
                           }

main :: IO ()
main = do
  args <- getArgs
  let settingsPath =
        case args of
          [] -> "data/game.json"
          path:_ -> path
  settings@(GameSettings {..}) <- do
    res <- JSON.eitherDecodeFileStrict' settingsPath
    case res of
      Left e -> fail e
      Right r -> return r

  let runCube = runStderrLoggingT

  runCube $ do
    let basePath = takeDirectory settingsPath
    modelsPromises <- liftIO $ mapM (async . TF.loadFiles . (basePath </>)) gameModels
    vertexShaderPromise <- liftIO $ async $ runCube $ readAndPreprocessShader (basePath </> gameVertexShader)
    fragShaderPromise <- liftIO $ async $ runCube $ readAndPreprocessShader (basePath </> gameFragmentShader)

    SDL.initialize [InitVideo, InitEvents]

    window <- createWindow "A Window" defaultWindow { windowInputGrabbed = False
                                                    , windowInitialSize = V2 (fromIntegral gameInitialWidth) (fromIntegral gameInitialHeight)
                                                    , windowGraphicsContext = OpenGLContext defaultOpenGL { glProfile = Core Debug 3 3 }
                                                    }

    setMouseLocationMode RelativeLocation

    glContext <- glCreateContext window
    glMakeCurrent window glContext

    giveContext $ do
      Right vertexShader <- liftIO $ wait vertexShaderPromise
      Right fragShader <- liftIO $ wait fragShaderPromise
      shaderCache <- newCubePipelineCache vertexShader fragShader
      models <- liftIO $ mapM wait modelsPromises
      modelNodes <- mconcat <$> mapM (loadNodes shaderCache) models

      let fov = gameFov * pi / 180
          initialState = GameState { stateCamera = mempty
                                   , stateScreen = perspectiveScreen fov (fromIntegral gameInitialWidth / fromIntegral gameInitialHeight) gameNearPlane gameFarPlane
                                   , stateNodes = modelNodes
                                   }
          gameWindow = GameWindow { gameWindow = window
                                  , gameFovRadians = fov
                                  }
          gameApp :: EventLoopApp GameState
          gameApp = EventLoopApp { appNetworkSetup = gameNetwork gameWindow initialState
                                 , appDrawFrame = drawFrame gameWindow
                                 , appFrameInterval = 1000 `div` fromIntegral gameFrameRate
                                 , appWorldInterval = 1000 `div` fromIntegral gameWorldRate
                                 }

      runInEventLoop gameApp

    glDeleteContext glContext
    destroyWindow window

drawFrame :: MonadCube m => GameWindow -> GameState -> m ()
drawFrame (GameWindow {..}) (GameState {..}) = do
  let prepared = prepareLoadedNodes stateNodes
      drawParams = defaultDrawParams { fragmentPassTests = defaultFragmentPassTests { cullFace = NoCulling
                                                                                    }
                                     }
  Caramia.clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ Caramia.rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer
  runDrawPreparedPipelines drawParams stateScreen stateCamera prepared
  glSwapWindow gameWindow
  runPendingFinalizers

gameNetwork :: (Reflex t, MonadCube m) => GameWindow -> GameState -> Event t CubeTickInfo -> Event t SDL.EventPayload -> m (Behavior t (Maybe GameState))
gameNetwork (GameWindow {..}) initialState tickEvent sdlEvent = return $ constant $ Just initialState


{- processEvent :: GameState -> SDL.Event -> Maybe GameState
processEvent state@(GameState {..}) event =
  case eventPayload event of
    WindowSizeChangedEvent (WindowSizeChangedEventData { windowSizeChangedEventSize = V2 width height }) ->
      Just state { stateScreen = perspectiveScreen stateFovRadians (fromIntegral width / fromIntegral height) (gameNearPlane stateSettings) (gameFarPlane stateSettings) }
    WindowClosedEvent _ -> Nothing
    QuitEvent -> Nothing
    _ -> Just state
-}
