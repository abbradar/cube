import Control.Monad.IO.Class
import Graphics.Rendering.OpenGL.Raw
import Control.Applicative
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.C.String
import Foreign.Storable
 
import Graphics.UI.SDL

main :: IO ()
main = withSDLAllVideo $ do
  setGLAttribute ContextMajorVersion 3
  setGLAttribute ContextMinorVersion 3
  setGLAttribute ContextProfile SdlGlContextProfileCore

  (Right w) <- createWindow "A Window" (P NoHint NoHint) (P 640 480) [SdlWindowOpengl]
  c <- createGLContext w
  glSetCurrent c
  liftIO $ do
    (major1, minor1) <- alloca $ \major_ptr -> alloca $ \minor_ptr -> do
      -- in case glGetIntegerv is completely broken, set initial values for
      -- major and minor pointers
      poke major_ptr 0
      poke minor_ptr 0
      glGetIntegerv gl_MAJOR_VERSION major_ptr
      glGetIntegerv gl_MINOR_VERSION minor_ptr
      (,) <$> peek major_ptr <*> peek minor_ptr
    putStrLn $ "glGetIntegerv gl_*_VERSION: " ++ show major1 ++ "." ++ show minor1
    ver_raw <- castPtr `fmap` glGetString gl_VERSION
    ver_cptr <- peekCString ver_raw
    putStrLn $ "glGetString gl_VERSION: " ++ ver_cptr
