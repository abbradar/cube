{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Storable (Storable(..))
import Text.InterpolatedString.Perl6 (q)
import Control.Monad.Loops (iterateUntil)
import Graphics.Caramia
  
import Graphics.UI.SDL.Monad
import Graphics.UI.SDL.Types
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Keyboard
import qualified Graphics.UI.SDL.Video.Window as W
import Graphics.UI.SDL.Video.OpenGL
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Events.Monad
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Timer.Monad

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = fromIntegral $ sizeOf (undefined :: a) * V.length vec

main :: IO ()
main = withSDL $ withSDLEvents $ withSDLTimer $ withSDLVideo $ do
  setGLAttribute ContextMajorVersion 3
  setGLAttribute ContextMinorVersion 3
  setGLAttribute ContextProfile SdlGlContextProfileCore

  (Right w) <- W.createWindow "A Window" (P W.Undefined W.Undefined) (P 640 480) [W.SdlWindowOpengl]
  stopTextInput
  c <- createGLContext w
  glSetCurrent c

  liftIO $ giveContext $ do
    pl <- newPipelineVF vxsource fgsource
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
  
  glSwap w
  void $ iterateUntil (\(Event _ ed) -> ed == Quit) waitEvent
  freeGLContext c
  W.freeWindow w

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
