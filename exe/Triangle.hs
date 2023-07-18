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
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.Vector as V
import System.FilePath
import Reflex
import Reflex.Host.Class
import SDL hiding (Event)

import Data.Aeson.Utils
import Data.GLSL.Preprocessor
import Cube.Types
import Cube.Time
import Cube.Loop.Reflex
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Graphics.Screen
import Cube.Graphics.Camera
import Cube.Graphics.Render
import Cube.Graphics.Scene.Resources
import Cube.Graphics.Scene.Runtime
import Cube.Graphics.Scene.Types
import Cube.Input.Events
import Cube.Input.Keyboard
import Cube.Input.Mouse
import Cube.Map
import Cube.ECS

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
                           , stateScene :: SceneGraph
                           , stateWorld :: World
                           , stateMap :: Map
                           }

data GameInitialState = GameInitialState { initialScene :: SceneGraph
                                         , initialWorld :: World
                                         , initialMap :: Map
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
        sceneNodes = V.singleton SceneNode { sceneNodeTranslation = Just gameInitialPosition
                                           , sceneNodeRotation = Just gameInitialRotation
                                           , sceneNodeScale = Just $ V3 1 1 1
                                           , sceneNodeModel = Just "player"
                                           , sceneNodeMatrix = Nothing
                                           , sceneNodeChildren = Just V.empty
                                           }
    initialScenePromise <- liftIO $ async $ runCube $ readSceneFiles (basePath </> gameScene) sceneNodes
    initialMapPromise <- liftIO $ async $ runCube $ readMapFiles (basePath </> gameMap)
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
      sceneGraph <- newSceneGraph $ SceneOptions { sceneVertexShader = vertexShader
                                                 , sceneFragmentShader = fragShader
                                                 }
      initialScene <- liftIO $ wait initialScenePromise
      (mpRnd, sceneMap') <- liftIO $ wait initialMapPromise
      sceneGraph' <- addScene sceneGraph initialScene
      let mp = mapFromRnd MapData{ mapRandom = mpRnd, mapPath =  gameMap } [V2 0 0, V2 0 (-1), V2 (-1) (-1), V2 (-1) 0]
          playerTrs = undefined
          iWorld = addEntity (HM.fromList [(0, Column {columnData = V.singleton $ Transform playerTrs}), (3, Column {columnData = V.singleton $ Player 0})]) (newWorld 0) --{ playerPos = gameInitialPosition, playerRotation = gameInitialRotation }
      sceneGraph'' <- addChunksToScene sceneGraph' mp sceneMap'

      let initialState = GameInitialState { initialScene = sceneGraph''
                                          , initialWorld = iWorld
                                          , initialMap = mp
                                          }
          gameWindow = GameWindow { gameWindow = window
                                  , gameFovRadians = gameFov * pi / 180
                                  , gameSettings = settings
                                  }
          gameApp :: EventLoopApp GameState GameExtra GameAction
          gameApp = EventLoopApp { eappNetworkSetup = gameNetwork gameWindow initialState
                                 , eappDrawFrame = \_extra frame -> drawGameFrame gameWindow frame
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

drawGameFrame :: MonadCube m => GameWindow -> GameState -> m ()
drawGameFrame (GameWindow {..}) (GameState {..}) = do
  ts <- SDL.ticks
  advanceAnimations ts stateScene
  prepared <- prepareSceneGraph stateScene
  Caramia.clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ Caramia.rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer
  runDrawPreparedNodes defaultDrawParams stateScreen stateCamera prepared
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
        V2 sx sy <- sample $ current windowSize
        let ratio = fromIntegral $ min sx sy
        return $ Just $ fmap fromIntegral move ^/ ratio
  let normalizedMouseMoveStep = push normalizedShift mouseMoveStep

  let updateCamera (mmove, mrotation) camera@(FloatingCamera {..}) = do
        let camera' =
              case mmove of
                Nothing -> camera
                Just step -> camera { fCameraTarget = fCameraTarget + 0.1 *^ rotate fCameraRotation step }
            camera'' =
              case mrotation of
                Nothing -> camera'
                Just shift -> fCameraRotateNoRoll shift camera'
        return camera''

  let updatePlayer (mmove, mrotation) wrld = do
        let
          movePlayer :: V3 Float -> Component -> Component
          movePlayer step (Transform trs) = Transform trs{ trsTranslation = trsTranslation trs + (0.1 *^ rotate (trsRotation trs) step) }
          movePlayer _ _ = error "wrong component identifier"
          wrld' =
              case mmove of
                Nothing -> wrld
                Just step -> updateComponentByType (HS.fromList [0,3]) 0 (movePlayer step) wrld--moveRotate pl (0.1 *^ rotate playerRotation step) (Quaternion 1.0 (V3 0.0 0.0 0.0)) initialMap
        return wrld'
  let kbdCameraStep = fmap ((, Nothing) . Just) kbdMoveStep
      mouseCameraStep = fmap ((Nothing, ) . Just) normalizedMouseMoveStep
      cameraStep = mergeWith (\(moveA, rotateA) (moveB, rotateB) -> (moveA <|> moveB, rotateA <|> rotateB)) [kbdCameraStep, mouseCameraStep]


  playerCamera <- foldDynM updateCamera (fCameraLookAt (gameInitialPosition + gameCameraShift) gameInitialPosition ) cameraStep
  world <- foldDynM updatePlayer initialWorld cameraStep

  let screen = fmap (\(V2 width height) -> perspectiveScreen gameFovRadians (fromIntegral width / fromIntegral height) gameNearPlane gameFarPlane) windowSize
      scene = constant initialScene
      mp = constant initialMap
     -- player = constant initialPlayer
      frameBehavior = GameState <$> current playerCamera <*> current screen <*> scene <*> current world <*> mp

      network = EventLoopNetwork { eloopQuitEvent = quitEvent
                                 , eloopFrameBehavior = frameBehavior
                                 }

  gameResizeHandle <- subscribeEvent resizeEvent
  let extra = GameExtra { gameResizeHandle
                        }

  return (extra, network)
