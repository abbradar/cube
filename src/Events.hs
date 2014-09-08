{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}

import FRP.Netwire
import Prelude hiding ((.), id)
import Control.Monad.Logger (logWarn)
import Control.Wire.Core (Wire (..))
import Control.Monad.Base (liftBase)
import Graphics.Rendering.OpenGL.GL.Tensor (Vector2(..))

import Graphics.UI.SDL.Monad
import Graphics.UI.SDL.Video.Monad
import Graphics.UI.SDL.Video.Keyboard
import qualified Graphics.UI.SDL.Video.Window as W
import Graphics.UI.SDL.Events.Monad
import Graphics.UI.SDL.Events.Types
import Graphics.UI.SDL.Timer.Monad
import Graphics.UI.SDL.Utils.Framerate
import FRP.Netwire.SDL
import FRP.Netwire.SDL.State
import FRP.Netwire.SDL.Types

main :: IO ()
main =
  withSDL $ withSDLEvents $ withSDLTimer $ withSDLVideo $ do
    $(logWarn) "hajimemashou"

    w <- W.createWindow "A Window" (Vector2 W.Undefined W.Undefined) (Vector2 640 480) []
    stopTextInput
    s <- newInternalState
    l <- newFPSLimiter

    let dloop f' = do
          (r', f) <- sdlStep s () f' (Right ())
          case r' of
           Left _ -> return ()
           Right r -> do
             limitFPS_ l $ 1000 `div` 2
             liftBase $ putStrLn $ show r
             dloop f

    dloop $ wire . time
    
    W.freeWindow w

  where wire = WPure $ \(stateData -> s@StateData { _rawEvents }) (Right t) ->
                        (if Quit `elem` _rawEvents then Left t else Right (t, s), wire)
