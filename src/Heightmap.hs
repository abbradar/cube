{-# LANGUAGE QuasiQuotes #-}

import Prelude hiding ((.), id, until)
import Control.Wire hiding (when)
import Data.Word
import Control.Monad
import Control.Monad.IO.Class
import Foreign.Storable (Storable(..))
import Foreign.ForeignPtr
import Text.InterpolatedString.Perl6 (q)
import Graphics.Caramia
import Linear.V3
import Linear.Matrix
import Linear.Projection
import qualified Data.Array.Repa as R
import Data.Array.Repa (Array, Source, Shape)
import Data.Array.Repa.Index
import qualified Data.Array.Repa.Repr.ForeignPtr as RF
  
import Graphics.UI.SDL
import Graphics.UI.SDL.Utils.Framerate
import FRP.Netwire.SDL

-- | Total size of an array (in bytes).
sizeS :: forall a r sh. (Storable a, Shape sh, Source r a) => Array r sh a -> Int
sizeS vec = sizeOf (undefined :: a) * R.size (R.extent vec)

-- | Upload an array to an OpenGL buffer.
uploadRepa :: (MonadIO m, Storable a, Shape sh, Source r a) => Array r sh a -> Int -> Buffer -> m ()
uploadRepa arr offset buff =
  liftIO $ withMapping offset (sizeS arr) WriteAccess buff $ \ptr -> do
    fp <- castForeignPtr <$> newForeignPtr_ ptr
    RF.computeIntoS fp $ R.delay arr

-- | A wire that emits its current state and changes it monadically on incoming event.
switchM :: (Monad m) => a -> ((a, b) -> m a) -> Wire s e m (UniqueEvent b) a
switchM s' m = loop s'
  where loop s = mkGenN $ eventU (return (Right s, loop s)) $ \e -> do
          ns <- m (s, e)
          return (Right ns, loop ns)

-- | Build an index map for a rectangular heightmap.
mapTriangles :: DIM2 -> Array R.D DIM2 Word32
mapTriangles (Z :. h :. w) = R.fromFunction (ix2 (w - 1) ((h - 1) * 2 * 3)) $
                             \(Z :. y :. x'') ->
                              let (x', t) = x'' `divMod` 3
                                  (x,  n) = x' `divMod` 2
                              in if | t == 0 -> if n == 0
                                               then pos y x
                                               else pos (y + 1) (x + 1)
                                    | t == 1 -> pos y (x + 1)
                                    | t == 2 -> pos (y + 1) x
                                    | otherwise -> undefined
  where pos y x = fromIntegral $ y * w + x

-- | Wire-based 'if'.
ifW :: Monoid e => Wire s e m (a, Bool) a
ifW = mkPure_ $ \(a, c) -> if c then Right a else Left mempty

main :: IO ()
main = withSDLAllVideo $ do
  setGLAttribute ContextMajorVersion 3
  setGLAttribute ContextMinorVersion 3
  setGLAttribute ContextProfile SdlGlContextProfileCore
  setGLAttribute ContextFlags [SdlGlContextDebugFlag]

  let isize = V2 640 480
  (Right w) <- createWindow "A Window" (V2 NoHint NoHint) isize [SdlWindowOpengl]
  stopTextInput

  s0 <- sdlSession
  l0 <- fpsSession
  c <- createGLContext w
  glSetCurrent c

  giveContext $ do
    pl <- newPipelineVF vxsource fgsource
    vao <- newVAO
    let indices = mapTriangles $ R.extent hmap
    ptsbuf <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                              , size = sizeS hmap
                                              }
    uploadRepa hmap 0 ptsbuf
    indbuf <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                              , size = sizeS indices
                                              }
    uploadRepa indices 0 indbuf
    tr <- getUniformLocation "transform" pl
    when (tr == -1) $ fail "transform matrix was not found"
    sourceVertexData ptsbuf defaultSourcing { components = 3
                                            , attributeIndex = 0
                                            , sourceType = SFloat
                                            } vao

    let
      mouseGrab = switchM False $ \(s, _) -> do
        setMouseGrab (not s) w
        setCursorShown s
        return $ not s

      draw = mkGen_ $ \mtx -> do
        setUniform mtx tr pl
        clear clearing { clearDepth = Just 0
                       , clearColor = Just $ rgba 0 0 0 0
                       } screenFramebuffer
        let drawp = defaultDrawParams { pipeline = pl
                                      , fragmentPassTests =
                                        defaultFragmentPassTests { cullFace = NoCulling
                                                                 }
                                      }
        runDraws drawp $ do
          drawR drawCommand { primitiveType = Triangles
                            , primitivesVAO = vao
                            , numIndices = R.size $ R.extent indices
                            , sourceData = PrimitivesWithIndices { indexBuffer = indbuf
                                                                 , indexOffset = 0
                                                                 , indexType = IWord32
                                                                 }
                            }
        glSwap w
        return $ Right ()

      game = until . (level &&& sdlOnEvent _Quit)

      mouseW = arr fromIntegral

      level = proc _ -> do
        -- On event of window resize (and on game start), resize viewport
        _ <- onUEventM (\(V2 w h) -> setViewportSize (fromIntegral w) (fromIntegral h)) .
             (now . pure isize &> sdlOnEvent_ (anyWindow . _Resized)) -< ()
        -- On the start of the game and when Esc is pressed toogle mouse grabbing
        esc <- mouseGrab . (now <!> onKey Pressed SdlkEscape) -< ()
        -- Process mouse movements when mouse is being grabbed.
        (mx, my) <- (mouseW *** mouseW) . (mouseMove Nothing . ifW <|> pure (0, 0)) -< ((), esc)

        -- Get window size, compute matrices.
        (V2 w h) <- sdlOnState $ anyWindowState . wsize -< ()
        let ratio = fromIntegral w / fromIntegral h
            frust = frustum (-ratio) ratio (-1) 1 1 10 :: M44 Float
        draw -< frust

      dloop s' l' f' = do
        (r', f, s) <- sdlStep s' () f' $ Right ()
        case r' of
         Left () -> return ()
         Right () -> do
           (_, l) <- fpsLimit l' $ 1000 `div` 5
           dloop s l f

    dloop s0 l0 game
  
  freeGLContext c
  freeWindow w

  where {-hmap = R.fromListUnboxed (ix2 3 3) $ map fromTuple3
               [ (0, 0, 0.0)
               , (0, 1, 0.2)
               , (0, 2, 0.5)
               , (1, 0, -0.3)
               , (1, 1, 0.05)
               , (1, 2, 0.2)
               , (2, 0, -0.5)
               , (2, 1, -0.2)
               , (2, 2, 0.1)
               ]-}
        hmap :: Array R.U DIM2 (V3 Float)
        hmap = R.fromListUnboxed (ix2 2 2) $ map fromTuple3
               [ (-0.2, -0.2, -1)
               , (-0.2, 0.2, -1.5)
               , (0.2, -0.2, -1.5)
               , (0.3, 0.3, -1.5)
               ]

        fromTuple3 (x, y, z) = V3 x y z

        vxsource =
          [q|
           #version 330 core

           layout(location = 0) in vec3 vpos;
           uniform mat4 transform;

           void main()
           {
             gl_Position.xyz = vpos;
             gl_Position.w = 1.0;
             gl_Position = transform * gl_Position;
           }
          |]

        fgsource =
          [q|
           #version 330 core
         
           out vec3 color;
 
           void main()
           {
             color = vec3(0.01 + 0.99 * gl_FragCoord.z, 0.01, 0.01);
           }
          |]
