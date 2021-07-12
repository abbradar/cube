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
import Reflex.Host.Class
import SDL hiding (Event)

import Data.Aeson.Utils
import qualified Data.GlTF.Resources as TF
import Data.GLSL.Preprocessor
import Cube.Types
import Cube.Loop.Stable
import Cube.Loop.Reflex
import Cube.Graphics.Types
import Cube.Graphics.Screen
import Cube.Graphics.Camera
import Cube.Graphics.Resources
import Cube.Graphics.Render
import Cube.Input.Keyboard

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
                             , gameSettings :: GameSettings
                             }

data GameState = GameState { stateCamera :: CameraF
                           , stateScreen :: ScreenF
                           , stateNodes :: LoadedNodes
                           }

data GameInitialState = GameInitialState { initialNodes :: LoadedNodes
                                         }

data GameExtra t = GameExtra { gameResizeHandle :: EventHandle t (V2 Int)
                             }

data GameAction = GameAction { gameResize :: Maybe (V2 Int)
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

    let myWindowSize = V2 (fromIntegral gameInitialWidth) (fromIntegral gameInitialHeight) :: V2 Int
    window <- createWindow "A Window" defaultWindow { windowInputGrabbed = False
                                                    , windowResizable = True
                                                    , windowInitialSize = fromIntegral <$> myWindowSize
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

      let initialState = GameInitialState { initialNodes = modelNodes
                                          }
          gameWindow = GameWindow { gameWindow = window
                                  , gameFovRadians = gameFov * pi / 180
                                  , gameSettings = settings
                                  }
          gameApp :: EventLoopApp GameState GameExtra GameAction
          gameApp = EventLoopApp { eappNetworkSetup = gameNetwork gameWindow initialState
                                 , eappDrawFrame = \_extra frame -> drawFrame gameWindow frame
                                 , eappReadAction = readExtra
                                 , eappInterpretAction = interpretExtra
                                 , eappFrameInterval = 1000 `div` fromIntegral gameFrameRate
                                 , eappWorldInterval = 1000 `div` fromIntegral gameWorldRate
                                 }

      runInEventLoop gameApp

    glDeleteContext glContext
    destroyWindow window

readExtra :: MonadReadEvent t m => GameExtra t -> m GameAction
readExtra (GameExtra {..}) = do
  gameResize <- readEvent gameResizeHandle >>= sequence
  return GameAction {..}

interpretExtra :: MonadCube m => GameAction -> m ()
interpretExtra (GameAction {..}) = do
  case gameResize of
    Nothing -> return ()
    Just (V2 width height) -> setViewportSize width height

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

gameNetwork :: forall t m. (Reflex t, MonadHold t m, MonadCube m, MonadSubscribeEvent t m) => GameWindow -> GameInitialState -> Event t CubeTickInfo -> Event t SDL.EventPayload -> m (GameExtra t, EventLoopNetwork t GameState)
gameNetwork (GameWindow { gameSettings = GameSettings {..}, ..}) (GameInitialState {..}) tickEvent sdlEvent = mdo
  let resizeEvent = flip push sdlEvent $ \case
        WindowSizeChangedEvent (WindowSizeChangedEventData { windowSizeChangedEventSize = sz }) -> return $ Just (fromIntegral <$> sz)
        _ -> return Nothing
  let quitEvent = flip push sdlEvent $ \case
        QuitEvent -> return $ Just ()
        _ -> return Nothing
  keysPressed <- pressedSDLKeys sdlEvent
  moveDirection <- keysDirection keysPressed
  let updateCamera (CubeTickInfo {..}) camera@(Camera {..}) = do
        dir <- sample $ current moveDirection
        let distance = fromIntegral ticksElapsed * 0.05
        return $ camera { cameraPosition = cameraPosition + distance *^ dir }
  camera <- foldDynM updateCamera mempty tickEvent
  windowSize <- holdDyn (V2 (fromIntegral gameInitialWidth) (fromIntegral gameInitialHeight)) resizeEvent

  let screen = fmap (\(V2 width height) -> perspectiveScreen gameFovRadians (fromIntegral width / fromIntegral height) gameNearPlane gameFarPlane) windowSize
      nodes = constant initialNodes

      frameBehavior = GameState <$> current camera <*> current screen <*> nodes

      network = EventLoopNetwork { eloopQuitEvent = quitEvent
                                 , eloopFrameBehavior = frameBehavior
                                 }

  gameResizeHandle <- subscribeEvent resizeEvent
  let extra = GameExtra { gameResizeHandle
                        }

  return (extra, network)
