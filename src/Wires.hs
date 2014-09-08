{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

import FRP.Netwire
import Prelude hiding ((.), id, until)
import Control.Monad.Logger (logWarn)
import Control.Wire.Core (Wire (..))
import Graphics.Rendering.OpenGL.GL
import Control.Monad.Base (liftBase)
import Debug.Trace
import Control.Wire

import Graphics.UI.SDL.Monad
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Keyboard
import Graphics.UI.SDL.Video.Keyboard.Types
import qualified Graphics.UI.SDL.Video.Window as W
import Graphics.UI.SDL.Events.Monad
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Timer.Monad
import Graphics.UI.SDL.Utils.Framerate
import Graphics.UI.SDL.Video.OpenGL
import FRP.Netwire.Extra
import FRP.Netwire.SDL
import FRP.Netwire.SDL.State
import FRP.Netwire.SDL.Wires
import FRP.Netwire.SDL.Types

main :: IO ()
main =
  withSDL $ withSDLEvents $ withSDLTimer $ withSDLVideo $ do
    $(logWarn) "hajimemashou2"

    (Right w) <- W.createWindow "A Window" (Vector2 W.Undefined W.Undefined) (Vector2 640 480) [W.SdlWindowOpengl]
    stopTextInput
    s <- newInternalState
    l <- newFPSLimiter
    c <- createGLContext w
    glSetCurrent c

    let dloop f' = do
          (r', f) <- sdlStep s () f' (Right ())
          case r' of
           Left () -> return ()
           Right (a, b) -> do
             liftBase $ do
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
             limitFPS_ l $ 1000 `div` 60
             dloop f

    dloop game
    
    freeGLContext c
    W.freeWindow w

  where game = until . (level &&& (sdlOnEvent _Quit <!> onKey Pressed SdlkEscape))
        level = keys [(SdlkA, -0.2), (SdlkD, 0.2)] 0 &&& keys [(SdlkS, -0.2), (SdlkW, 0.2)] 0
        keys k l = integral 0 . keys' k l
          where keys' [] l = pure l
                keys' ((k, v):t) l = (pure v . whileKey Pressed k <|> 0) + keys' t l
