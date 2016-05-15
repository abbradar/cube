import Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Engine.Camera
import Data.Default
import Debug.Trace
import Linear.Matrix
import Linear.V3
import Engine.Mesh
--import Data.Vector.Storable (Vector)
--import qualified Data.Vector.Storable as V
--import Foreign.Storable (Storable(..))
import Text.InterpolatedString.Perl6 (q)
--import Control.Monad.Loops (iterateUntil)
import Graphics.Caramia
import SDL


main :: IO ()
main = do
  initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 } }
  c <- glCreateContext w
  glMakeCurrent w c

  liftIO $ giveContext $ do
    pl <- newPipelineVF vxsource fgsource M.empty

    trimesh <- loadMeshObj "src/Data/pyramid.obj"
    meshbuff <- initMesh trimesh
 
    glSwapWindow w
    drawloop pl meshbuff w

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


drawloop :: Pipeline -> MeshBuffer -> Window -> IO ()
drawloop pl meshbuff w = 
  do
    events <- pollEvents
    let noexitEv = (null (filter (\e -> eventPayload e == QuitEvent) events))
    when noexitEv $ do
      Graphics.Caramia.clear clearing { clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                                      } screenFramebuffer
      runDraws defaultDrawParams { pipeline = pl } $ do
        pMloc <- getUniformLocation "projectionMat" pl
        setUniform pM pMloc pl
        drawMesh meshbuff pl mvM

      runPendingFinalizers
      glSwapWindow w
      drawloop pl meshbuff w
  where mvM :: M44 Float
        pM :: M44 Float   
        cam = def {eye = (V3 (10.0) 0.0 (12.0))} :: Camera
        mvM = viewMatrix cam
        pM =  projectionMatrix cam

