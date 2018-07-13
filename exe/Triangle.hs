import Data.Int
import Data.Word
import Data.Monoid
import qualified Data.Text.IO as T
import System.Environment
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Default
import Linear
import Control.Lens
import Control.Exception (handle)
import Graphics.Caramia hiding (normalize, draw)
import qualified Graphics.Caramia as Car
import SDL hiding (initialize)
import qualified SDL 

import Engine.Drawable
import Engine.Camera
import Engine.Framerate
import Engine.Mesh
import Data.DirectX

import Debug.Trace

data GameSettings = GameSettings { intervalTime :: Word32
                                 , initialSize :: V2 Int32
                                 , movementSpeed :: Float
                                 , mouseSensitivity :: Float
                                 , xTemplatesPath :: FilePath
                                 , shaderPath :: FilePath
                                 , sshaderPath :: FilePath
                                 , objPath :: FilePath
                                 }
                  deriving (Show, Read, Eq)

data DirectionalLight = DirectionalLight { lcolor :: V3 Float
                                         , ldirection :: V3 Float
                                         , lambient :: Float
                                         }
                  deriving (Show, Read, Eq)

data GameInitialState = GameInitialState { pl :: Pipeline
--                                         , pls :: Pipeline
                                         , object :: Object
                                         , sobject :: Object
                                         , skeleton :: FrameTree
                                         , light :: DirectionalLight
                                         , fpsLimit :: FPSLimit
                                         , xDataTemplates :: XTemplates
                                         }

data GameState = GameState { _camera :: Camera
                           , _leftButton :: Bool
                           , _pressedKeys :: Set Keycode
                           , _movedMouse :: V2 Int32
                           , _frameSize :: V2 Int32
                           , _frameTime :: Word32
                           }

$(makeLenses ''GameState)

main :: IO ()
main = do
  args <- getArgs
  let settingsPath = case args of
        [] -> "data/game.cfg"
        path:_ -> path
  settings@(GameSettings {..}) <- read <$> readFile settingsPath

  xDataTemplates <- xTemplates <$> parseFromFile (directX False) xTemplatesPath

  SDL.initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { windowInitialSize = fromIntegral <$> initialSize
                                             , windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 }
                                             }
  c <- glCreateContext w
  glMakeCurrent w c

  giveContext $ do
    vxsource <- T.readFile $ sshaderPath <> ".vs"
    fgsource <- T.readFile $ sshaderPath <> ".fs"
    pl <- handle (\(ShaderCompilationError msg) -> T.putStrLn msg >> fail "shader compilation error") $ newPipelineVF vxsource fgsource M.empty
    -- .X files
    objd <- loadFromFile xDataTemplates "data/xobjects/" "lzom.x" False
    object <- initializeI objd

    sobjd <- loadFromFile xDataTemplates "data/xobjects/" "lzom.x" True
    skeleton <- loadFrameIX xDataTemplates "data/xobjects/lzomsk.x"
    sobject <- initializeS sobjd skeleton
    --light
    let light = DirectionalLight { lcolor = V3 0.7 0.7 0.7
                                 , ldirection = V3 (-0.42) (-0.57) (-0.71)
                                 , lambient = 0.6
                                 }
    fpsLimit <- newFPSLimit
 
    let initialState = GameInitialState {..}
        state0 = GameState { _camera = def
                           , _pressedKeys = S.empty
                           , _leftButton = False
                           , _frameSize = initialSize
                           , _frameTime = intervalTime
                           , _movedMouse = V2 0 0
                           }
    drawLoop w settings initialState state0

  glDeleteContext c
  destroyWindow w

          
drawLoop :: Window -> GameSettings -> GameInitialState -> GameState -> IO ()
drawLoop w (GameSettings {..}) (GameInitialState {..}) = loop
  where
    loop st = do
      events <- pollEvents
      let mst = foldl processEvent (Just st { _movedMouse = V2 0 0 }) events
      case mst of
        Nothing -> return ()
        Just st' -> do
          let st'' = updateState st'
          doDraw st''
          ftime <- fpsDelay fpsLimit intervalTime
          loop (st'' & frameTime .~ ftime)

    processEvent Nothing _ = Nothing
    processEvent (Just st) (Event {..}) = case eventPayload of
      QuitEvent -> Nothing
      -- FIXME: update camera
      -- XXX: maybe WindowSizeChangedEvent is more proper here?
      WindowResizedEvent (WindowResizedEventData {..}) -> Just (st & frameSize .~ windowResizedEventSize)
      MouseMotionEvent (MouseMotionEventData {..}) | _leftButton st -> Just (st & movedMouse %~ (+ mouseMotionEventRelMotion))
      KeyboardEvent (KeyboardEventData {..}) -> case keyboardEventKeyMotion of
        Pressed -> Just (st & pressedKeys %~ S.insert (keysymKeycode keyboardEventKeysym))
        Released -> Just (st & pressedKeys %~ S.delete (keysymKeycode keyboardEventKeysym))
      MouseButtonEvent (MouseButtonEventData {..}) -> case mouseButtonEventMotion of
        Pressed -> Just (st & leftButton .~ True)
        Released -> Just (st & leftButton .~ False)
      _ -> Just st

    updateState = updatePos . updateLook

    updatePos st@(GameState {..}) = st & camera %~ moveEyes (movementSpeed * fromIntegral _frameTime *^ delta)
      where delta = normalize $ fwd + back + left + right

            moveKey k v
              | k `S.member` _pressedKeys = v
              | otherwise = V3 0 0 0

            fwd = moveKey KeycodeW (V3 0 1 0)
            back = moveKey KeycodeS (V3 0 (-1) 0)
            left = moveKey KeycodeA (V3 (-1) 0 0)
            right = moveKey KeycodeD (V3 1 0 0)

    updateLook st@(GameState {..}) =
      st & camera %~ rotateEyes (mouseSensitivity *^ V2 2 (-2) * (fromIntegral <$> _movedMouse) / (fromIntegral <$> _frameSize))

    doDraw (GameState {..}) = do
      let mvM = viewMatrix _camera
          pM = projectionMatrix _camera

      -- print (_eye _camera, _rotation _camera)
      -- FIXME: set viewpoint size according to window size
      Car.clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer
      -- start drawing
      runDraws defaultDrawParams { pipeline = pl } $ do
        let 
        -- depth
        setFragmentPassTests defaultFragmentPassTests { depthTest = Just Less }
        -- set matrices
        pMloc <- getUniformLocation "projectionMat" pl
        setUniform pM pMloc pl
--        mvMloc <- getUniformLocation "modelViewMat" pl
--        setUniform mvM mvMloc pl

        bLoc <- getUniformLocation "modelViewMat" pl
        
        tloc <- getUniformLocation "tex" pl
        -- lighting
        lightcloc <- getUniformLocation "sunLight.color" pl
        setUniform (lcolor light) lightcloc pl
        lightdloc <- getUniformLocation "sunLight.direction" pl
        setUniform (ldirection light) lightdloc pl
        lightiloc <- getUniformLocation "sunLight.ambient" pl
        setUniform (lambient light) lightiloc pl
        -- meshes
--        draw DContext {cpl = pl, cmvMLoc = mvMloc, ctexLoc = tloc, cmvM = mvM} object
        drawS DSContext {cspl = pl, cBnesLocation = bLoc, cstexLoc = tloc, cvM = mvM} sobject skeleton
      runPendingFinalizers
      glSwapWindow w