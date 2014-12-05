import FRP.Netwire
import Prelude hiding ((.), id, until)
import Graphics.Rendering.OpenGL.GL
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.SDL
import Graphics.UI.SDL.Utils.Framerate
import FRP.Netwire.SDL

main :: IO ()
main = withSDLAllVideo $ do
    (Right w) <- createWindow "A Window" (P NoHint NoHint) (P 640 480) [SdlWindowOpengl]
    stopTextInput
    s0 <- sdlSession
    l0 <- fpsSession
    c <- createGLContext w
    glSetCurrent c

    let dloop s' l' f' = do
          (r', f, s) <- sdlStep s' () f' $ Right ()
          case r' of
           Left () -> return ()
           Right (a, b) -> do
             liftIO $ do
               clear [ColorBuffer]
               renderPrimitive Quads $ do
                 color $ Color3 1 1 (1 :: GLfloat)
                 mapM_ (\(x, y, z) -> vertex $ Vertex3 (x + a) (y + b) z)
                   [ (0, 0, 0 :: GLfloat)
                   , (0, 0.2, 0)
                   , (0.2, 0.2, 0)
                   , (0.2 ,0, 0)
                   ]
             glSwap w
             (_, l) <- fpsLimit l' $ 1000 `div` 60
             dloop s l f

    dloop s0 l0 game
    
    freeGLContext c
    freeWindow w

  where game = until . (level &&& (sdlOnEvent _Quit <!> onKey Pressed SdlkEscape))
        level = keys [(SdlkA, -0.2), (SdlkD, 0.2)] 0 &&& keys [(SdlkS, -0.2), (SdlkW, 0.2)] 0
        keys k' l = integral 0 . keys' k'
          where keys' [] = pure l
                keys' ((k, v):t) = (pure v . whileKey Pressed k <|> 0) + keys' t
