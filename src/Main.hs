import Data.Word
import System.Environment
import qualified Data.Map as M
import Data.Default
import Linear.Matrix
import Linear.V3
import Engine.Mesh
import Text.InterpolatedString.Perl6 (q)
import Graphics.Caramia
import SDL

import Engine.Camera
import Engine.Framerate

import Debug.Trace

data GameSettings = GameSettings { meshPath :: FilePath
                                 , intervalTime :: Word32
                                 }
                  deriving (Show, Read, Eq)

data GameInitialState = GameInitialState { pl :: Pipeline
                                         , meshBuffer :: MeshBuffer
                                         , fpsLimit :: FPSLimit
                                         }

data GameState = GameState { }

main :: IO ()
main = do
  args <- getArgs
  let settingsPath = case args of
        [] -> "data/game.cfg"
        path:_ -> path
  settings@(GameSettings {..}) <- read <$> readFile settingsPath

  initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 } }
  c <- glCreateContext w
  glMakeCurrent w c

  giveContext $ do
    pl <- newPipelineVF vxsource fgsource M.empty

    mesh <- loadMesh meshPath
    meshBuffer <- initMeshBuffer mesh
    fpsLimit <- newFPSLimit
 
    let initialState = GameInitialState {..}
        state0 = GameState { }
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
          doDraw st'
          fpsDelay fpsLimit intervalTime
          loop st'

    processEvent Nothing _ = Nothing
    processEvent (Just st) (Event {..}) = case eventPayload of
      QuitEvent -> Nothing
      -- TODO: do something
      KeyboardEvent kd -> Just st
      _ -> Just st

    doDraw st = do
      Graphics.Caramia.clear clearing { clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                                      } screenFramebuffer
      runDraws defaultDrawParams { pipeline = pl } $ do
        pMloc <- getUniformLocation "projectionMat" pl
        setUniform pM pMloc pl
        drawMesh meshBuffer pl mvM

      runPendingFinalizers
      glSwapWindow w

    mvM :: M44 Float
    pM :: M44 Float
    cam = def {eye = (V3 (10.0) 0.0 (12.0))} :: Camera
    mvM = viewMatrix cam
    pM =  projectionMatrix cam

