import FRP.Netwire
import Prelude hiding ((.), id)
import Control.Wire.Core (Wire (..))
import Control.Monad.IO.Class (liftIO)

import Graphics.UI.SDL.Types
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
    w <- W.createWindow "A Window" (P W.Undefined W.Undefined) (P 640 480) []
    stopTextInput
    s0 <- sdlSession
    l0 <- fpsSession

    let dloop s' l' f' = do
          (r', f, s) <- sdlStep s' () f' (Right ())
          case r' of
           Left _ -> return ()
           Right r -> do
             (_, l) <- fpsLimit l' $ 1000 `div` 2
             liftIO $ putStrLn $ show r
             dloop s l f

    dloop s0 l0 $ wire . time
    
    W.freeWindow w

  where wire = WPure $ \(stateData -> s@StateData { _rawEvents }) (Right t) ->
                        (if Quit `elem` _rawEvents then Left t else Right (t, s), wire)
