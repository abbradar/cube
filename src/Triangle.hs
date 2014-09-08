{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad.Base (MonadBase(..))
import Control.Monad.Logger (logWarn, logDebug)
import Data.Function (fix)
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as V
import Foreign.Ptr (nullPtr)
import Foreign.Storable (Storable, sizeOf)
import Foreign.C.Types (CPtrdiff)
import qualified Data.Text as T
import Graphics.Rendering.OpenGL.GL
  
import Graphics.Rendering.OpenGL.Safe.Shaders
import Graphics.UI.SDL.Monad
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Keyboard
import qualified Graphics.UI.SDL.Video.Window as W
import Graphics.UI.SDL.Video.OpenGL
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Events.Monad
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Timer.Monad

import Multiline

storableSize :: (Storable a) => Vector a -> CPtrdiff
storableSize a = fromIntegral $ sizeOf (V.head a) * V.length a

main :: IO ()
main = withSDL $ withSDLEvents $ withSDLTimer $ withSDLVideo $ do
  $(logWarn) "hajimemashou"
  setGLAttribute ContextMajorVersion 3
  setGLAttribute ContextMinorVersion 3
  setGLAttribute ContextProfile SdlGlContextProfileCore

  liftBase $ putStrLn "dick"
  (Right w) <- W.createWindow "A Window" (Vector2 W.Undefined W.Undefined) (Vector2 640 480) [W.SdlWindowOpengl]
  stopTextInput
  c <- createGLContext w
  glSetCurrent c

  pr <- liftBase createProgram
  prepareShader VertexShader vxsource >>= liftBase . attachShader pr
  prepareShader FragmentShader fgsource >>= liftBase . attachShader pr
  safeLinkProgram pr

  liftBase $ do
    vao <- liftBase genObjectName
    liftBase $ bindVertexArrayObject $= Just vao
  
    vx <- liftBase genObjectName
    bindBuffer ArrayBuffer $= Just vx
    V.unsafeWith triangle $ \ptr ->
      bufferData ArrayBuffer $= (storableSize triangle, ptr, StaticDraw)
    bindBuffer ArrayBuffer $= Nothing

  -- Drawing now
    clear [ColorBuffer]
    currentProgram $= Just pr

    let attr = AttribLocation 0
    vertexAttribArray attr $= Enabled
    bindBuffer ArrayBuffer $= Just vx
    vertexAttribPointer attr $= (ToFloat, VertexArrayDescriptor 3 Float 0 nullPtr)
    drawArrays Triangles 0 3
    vertexAttribArray attr $= Disabled
      
  glSwap w
  fix $ \next -> do
    ev@(Event _ ed) <- waitEvent
    $(logDebug) $ T.pack $ show ev
    case ed of
      Quit -> return ()
      _ -> next
      
  freeGLContext c
  W.freeWindow w

  where triangle :: Vector GLfloat
        triangle = V.fromList [-1.0, -1.0, 0.0,
                               1.0, -1.0, 0.0,
                               0.0, 1.0, 0.0]
        vxsource =
          [multiline|
           #version 330 core

           layout(location = 0) in vec3 vertexPosition_modelspace;

           void main()
           {
             gl_Position.xyz = vertexPosition_modelspace;
             gl_Position.w = 1.0;
           }
          |]
        fgsource =
          [multiline|
           #version 330 core
         
           out vec3 color;
 
           void main()
           {
             color = vec3(1, 0, 0);
           }
          |]
