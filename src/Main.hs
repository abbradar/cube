import Data.Int
import Data.Word
import System.Environment
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Default
import Linear
import Text.InterpolatedString.Perl6 (q)
import Control.Lens
import Graphics.Caramia as Car hiding (normalize)
import SDL

import Engine.Mesh
import Engine.Camera
import Engine.Framerate
import Game.Types

import Debug.Trace

data GameSettings = GameSettings { meshPath :: FilePath
                                 , intervalTime :: Word32
                                 , initialSize :: V2 Int32
                                 , movementSpeed :: Float
                                 , mouseSensitivity :: Float
                                 }
                  deriving (Show, Read, Eq)

data GameInitialState = GameInitialState { pl :: Pipeline
                                         , meshBuffer :: MeshBuffer
                                         , fpsLimit :: FPSLimit
                                         }

main :: IO ()
main = do
  args <- getArgs
  let settingsPath = case args of
        [] -> "data/game.cfg"
        path:_ -> path
  settings@(GameSettings {..}) <- read <$> readFile settingsPath

  initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { windowInitialSize = fromIntegral <$> initialSize
                                             , windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 }
                                             }
  c <- glCreateContext w
  glMakeCurrent w c

  giveContext $ do
    pl <- newPipelineVF vxsource fgsource M.empty

    mesh <- loadMesh meshPath
    meshBuffer <- initMeshBuffer mesh
    fpsLimit <- newFPSLimit
 
    let initialState = GameInitialState {..}
        state0 = GameState { _camera = def
                           , _pressedKeys = S.empty
                           , _frameSize = initialSize
                           , _frameTime = intervalTime
                           , _movedMouse = V2 0 0
                           }
    drawLoop w settings initialState state0

  glDeleteContext c
  destroyWindow w

  where vxsource =
          [q|
           #version 330 core

           uniform mat4 projectionMat;
           uniform mat4 modelViewMat;

           layout(location = 0) in vec3 vpos;
           layout(location = 1) in vec3 col;
           
           smooth out vec3 ocol;
            
           void main()
           {
             ocol = col;
             gl_Position = projectionMat * modelViewMat * vec4(vpos, 1.0);
           }
          |]
        fgsource =
          [q|
           #version 330 core
           
           smooth in vec3 ocol;
           out vec4 color;
 
           void main()
           {
             color = vec4(ocol, 1.0);
           }
          |]

drawLoop :: Window -> GameSettings -> GameInitialState -> GameState -> IO ()
drawLoop w (GameSettings {..}) (GameInitialState {..}) = loop
  where
    loop st = do
      events <- pollEvents
      let mst = foldl processEvent (Just st) events
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
      MouseMotionEvent (MouseMotionEventData {..}) -> Just (st & movedMouse %~ (+ mouseMotionEventRelMotion))
      KeyboardEvent (KeyboardEventData {..}) -> case keyboardEventKeyMotion of
        Pressed -> Just (st & pressedKeys %~ S.insert (keysymKeycode keyboardEventKeysym))
        Released -> Just (st & pressedKeys %~ S.delete (keysymKeycode keyboardEventKeysym))
      _ -> Just st

    updateState = updatePos . updateLook

    updatePos st@(GameState {..}) = st & camera %~ moveEyes (movementSpeed * fromIntegral _frameTime *^ delta)
      where delta = normalize $ fwd + back + left + right

            moveKey k v
              | k `S.member` _pressedKeys = v
              | otherwise = V3 0 0 0

            fwd = moveKey KeycodeW (V3 0 0 1)
            back = moveKey KeycodeS (V3 0 0 (-1))
            left = moveKey KeycodeA (V3 (-1) 0 0)
            right = moveKey KeycodeD (V3 1 0 0)

    updateLook st@(GameState {..}) =
      st & camera %~ rotateEyes (mouseSensitivity *^ V2 2 (-2) * (fromIntegral <$> _movedMouse) / (fromIntegral <$> _frameSize))
         & movedMouse .~ V2 0 0

    doDraw (GameState {..}) = do
      let mvM = viewMatrix _camera
          pM = projectionMatrix _camera

      print (_eye _camera, _rotation _camera)
      -- FIXME: set viewpoint size according to window size
      Car.clear clearing { clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer
      runDraws defaultDrawParams { pipeline = pl } $ do
        pMloc <- getUniformLocation "projectionMat" pl
        setUniform pM pMloc pl
        drawMesh meshBuffer pl mvM

      runPendingFinalizers
      glSwapWindow w
