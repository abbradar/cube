{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import qualified Data.Map as M
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(..))
import Text.InterpolatedString.Perl6 (q)
import Control.Monad.Loops (iterateUntil)
import Graphics.Caramia
import SDL

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = fromIntegral $ sizeOf (undefined :: a) * V.length vec

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
                                            , size = sizeOfV triangle
                                            }
    uploadVector triangle 0 buff
    sourceVertexData buff defaultSourcing { components = 3
                                          , attributeIndex = 0
                                          , sourceType = SFloat
                                          } vao
    runDraws defaultDrawParams { pipeline = pl } $ do
      drawR drawCommand { primitiveType = Triangles
                        , primitivesVAO = vao
                        , numIndices = 3
                        , sourceData = Primitives 0
                        }
  
  glSwapWindow w
  void $ iterateUntil (\e -> eventPayload e == QuitEvent) waitEvent
  glDeleteContext c
  destroyWindow w

  where triangle :: Vector Float
        triangle = V.fromList [-1.0, -1.0, 0.0
                              , 1.0, -1.0, 0.0
                              , 0.0, 1.0, 0.0
                              ]
        vxsource =
          [q|
           #version 330 core

           layout(location = 0) in vec3 vpos;

           void main()
           {
             gl_Position.xyz = vpos;
             gl_Position.w = 1.0;
             gl_Position = gl_Position;
           }
          |]
        fgsource =
          [q|
           #version 330 core
         
           out vec3 color;
 
           void main()
           {
             color = vec3(1, 0, 0);
           }
          |]
