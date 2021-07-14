{-# LANGUAGE StrictData #-}

import GHC.Generics (Generic)
import System.Environment
import Data.Aeson as JSON
import Control.Concurrent.Async
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Logger
import Graphics.Caramia as Caramia
import Data.Functor.Misc
import System.FilePath
import Reflex
import Reflex.Host.Class
import SDL hiding (Event)

import Data.Aeson.Utils
import qualified Data.GlTF.Resources as TF
import Data.GLSL.Preprocessor
import Cube.Types
import Cube.Time
import Cube.Loop.Reflex
import Cube.Graphics.Types
import Cube.Graphics.Screen
import Cube.Graphics.Camera
import Cube.Graphics.Resources
import Cube.Graphics.Render
import Cube.Input.Events
import Cube.Input.Keyboard
import Cube.Input.Mouse

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

gameNetwork :: forall t m. (Reflex t, MonadHold t m, MonadCube m, MonadSubscribeEvent t m) => GameWindow -> GameInitialState -> Event t TimeStep -> Event t (TimeStep, SDL.EventPayload) -> m (GameExtra t, EventLoopNetwork t GameState)
gameNetwork (GameWindow { gameSettings = GameSettings {..}, ..}) (GameInitialState {..}) tickEvent sdlEvent = mdo
  let sdlEvents = fanSDLEvent sdlEvent
      kbEvents = subfanKeyboardEvent sdlEvents

  let resizeEvent = fmap (\(_time, info) -> fromIntegral <$> windowSizeChangedEventSize info) $ select sdlEvents WindowSizeChangedEventKey
      quitWindowEvent = const () <$> select sdlEvents QuitEventKey
      quitKeyEvent = const () <$> select kbEvents (Const2 SDL.KeycodeEscape)
      quitEvent = leftmost [quitWindowEvent, quitKeyEvent]

  windowSize <- holdDyn (V2 (fromIntegral gameInitialWidth) (fromIntegral gameInitialHeight)) resizeEvent
  kbdMoveStep <- normalizedMove tickEvent kbEvents
  mouseMoveStep <- relativeMovePerTick tickEvent (select sdlEvents MouseMotionEventKey)

  let normalizedShift move = do
        size <- sample $ current windowSize
        return $ Just $ fmap fromIntegral move / fmap fromIntegral size
  let normalizedMouseMoveStep = push normalizedShift mouseMoveStep
  
  let updateCamera (mmove, mrotation) camera@(Camera {..}) = do
        let camera' =
              case mmove of
                Nothing -> camera
                Just step -> camera { cameraPosition = cameraPosition + 0.05 *^ step }
            camera'' =
              case mrotation of
                Nothing -> camera'
                Just shift -> cameraRotateNoRoll shift camera'
        return camera''

  let kbdCameraStep = fmap ((, Nothing) . Just) kbdMoveStep
      mouseCameraStep = fmap ((Nothing, ) . Just) normalizedMouseMoveStep
      cameraStep = mergeWith (\(moveA, rotateA) (moveB, rotateB) -> (moveA <|> moveB, rotateA <|> rotateB)) [kbdCameraStep, mouseCameraStep]

  playerCamera <- foldDynM updateCamera mempty cameraStep

  let screen = fmap (\(V2 width height) -> perspectiveScreen gameFovRadians (fromIntegral width / fromIntegral height) gameNearPlane gameFarPlane) windowSize
      nodes = constant initialNodes

      frameBehavior = GameState <$> current playerCamera <*> current screen <*> nodes

      network = EventLoopNetwork { eloopQuitEvent = quitEvent
                                 , eloopFrameBehavior = frameBehavior
                                 }

  gameResizeHandle <- subscribeEvent resizeEvent
  let extra = GameExtra { gameResizeHandle
                        }

  return (extra, network)
