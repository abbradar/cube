import Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Control.Monad.IO.Class (MonadIO)
import Linear.Matrix
import Linear.Projection
import Linear.V3
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(..))
import Text.InterpolatedString.Perl6 (q)
--import Control.Monad.Loops (iterateUntil)
import Graphics.Caramia
import SDL

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * V.length vec

main :: IO ()
main = do
  initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 } }
  c <- glCreateContext w
  glMakeCurrent w c

  liftIO $ giveContext $ do
    pl <- newPipelineVF vxsource fgsource M.empty
    vao <- newVAO
    buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                            , size = sizeOfV triangle + sizeOfV trianglecol
                                            }
    uploadVector triangle 0 buff
    sourceVertexData buff defaultSourcing { components = 3
                                          , attributeIndex = 0
                                          , sourceType = SFloat
                                          } vao

    uploadVector trianglecol (sizeOfV triangle) buff
    sourceVertexData buff defaultSourcing { offset = (sizeOfV triangle)
                                          ,components = 3
                                          , attributeIndex = 1
                                          , sourceType = SFloat
                                          } vao
    glSwapWindow w
    drawloop pl vao w

  glDeleteContext c
  destroyWindow w

  where triangle :: Vector Float
        triangle = V.fromList [-1.0, -1.0, 0.0
                              , 1.0, -1.0, 0.0
                              , 0.0, 1.0, 0.0
                              ]
        trianglecol :: Vector Float
        trianglecol = V.fromList [1.0, 0.0, 0.0
                               , 0.0, 1.0, 0.0
                               , 0.0, 0.0, 1.0
                               ]
        vxsource =
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


drawloop :: Pipeline -> VAO -> Window -> IO ()
drawloop pl vao w = 
  do
    events <- pollEvents
    let noexitEv = (null (filter (\e -> eventPayload e == QuitEvent) events))
    when noexitEv $ do
      Graphics.Caramia.clear clearing { clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                                      } screenFramebuffer
      runDraws defaultDrawParams { pipeline = pl } $ do
        mvMloc <- getUniformLocation "modelViewMat" pl
        pMloc <- getUniformLocation "projectionMat" pl
        setUniform mvM mvMloc pl
        setUniform pM pMloc pl

        drawR drawCommand { primitiveType = Triangles
                          , primitivesVAO = vao
                          , numIndices = 3
                          , sourceData = Primitives 0
                          }
      runPendingFinalizers
      glSwapWindow w
      drawloop pl vao w
  where mvM :: M44 Float
        pM :: M44 Float   
        mvM = transpose ( lookAt (V3 (-10.0) 0.0 (12.0)) (V3 0.0 0.0 0.0) (V3 0.0 0.1 0.0))
        pM = transpose (perspective (pi/4.0) (4.0/3.0) 0.1 1000.0)

