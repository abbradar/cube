import Data.Int
import Data.Word
import Data.Monoid
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment
import qualified Data.Map as M
import qualified Data.Maybe as May
import Data.Set (Set)
import qualified Data.Set as S
import Data.Default
import Linear
import Control.Lens
import Control.Exception (handle)
import Control.Monad.IO.Class
import Graphics.Caramia hiding (normalize, draw)
import qualified Graphics.Caramia as Car
import SDL hiding (initialize)
import qualified SDL
--import SDL.Time (ticks)

import Engine.Drawable
import Engine.Camera
import Engine.Framerate
import Engine.Mesh
import Engine.Map
import Engine.Chunk
import Engine.Loaders
import Engine.Types
  
import Data.DirectX

import Debug.Trace

data GameSettings = GameSettings { intervalTime :: Word32
                                 , initialSize :: V2 Int32
                                 , movementSpeed :: Float
                                 , mouseSensitivity :: Float
                                 , xTemplatesPath :: FilePath
                                 , shaderPath :: FilePath
                                 , sshaderPath :: FilePath
                                 , objPath :: FilePath
                                 , mapLoadRadius :: Int
                                 }
                  deriving (Show, Read, Eq)

data DirectionalLight = DirectionalLight { lcolor :: V3 Float
                                         , ldirection :: V3 Float
                                         , lambient :: Float
                                         }
                  deriving (Show, Read, Eq)

data GameInitialState = GameInitialState { pls :: Pipelines
--                                         , pls :: Pipeline
                                         , object :: Object
                                         , sobject :: Object
                                         , anim1 :: [Animation]
                                        -- , gmap :: Map
                                        -- , mapBuffer :: MapBuffer
                                         , skeleton :: FrameTree
                                         , light :: DirectionalLight
                                         , fpsLimit :: FPSLimit
                                         , xDataTemplates :: XTemplates
                                         }

data GameState = GameState { _camera :: Camera
                           , _gmap :: Map
                           , _mapBuffer :: MapBuffer
                           , _leftButton :: Bool
                           , _pressedKeys :: Set Keycode
                           , _movedMouse :: V2 Int32
                           , _frameSize :: V2 Int32
                           , _frameTime :: Word32
                           }

$(makeLenses ''GameState)

newPipelineDebugVF :: MonadIO m => T.Text -> T.Text -> AttributeBindings -> m Pipeline
newPipelineDebugVF vert_src frag_src bindings = do
  vsh <- newShader vert_src Vertex
  vsh_log <- getShaderLog vsh
  when (T.length vsh_log /= 0) $ liftIO $ T.putStrLn vsh_log
  fsh <- newShader frag_src Fragment
  fsh_log <- getShaderLog fsh
  when (T.length fsh_log /= 0) $ liftIO $ T.putStrLn fsh_log
  pl <- newPipeline [vsh, fsh] bindings
  pl_log <- getPipelineLog pl
  when (T.length pl_log /= 0) $ liftIO $ T.putStrLn pl_log
  return pl


-------------------------------------------------------------------------------------------------------
------------------------------------------ GAME INIT --------------------------------------------------
-------------------------------------------------------------------------------------------------------
  
main :: IO ()
main = do
  args <- getArgs
  let settingsPath = case args of
        [] -> "data/game.cfg"
        path:_ -> path
  settings@(GameSettings {..}) <- read <$> readFile settingsPath

  xDataTemplates <- xTemplates <$> parseFromFile (directX False) xTemplatesPath

  SDL.initialize [InitVideo, InitEvents]

  w <- createWindow "A Window" defaultWindow { -- windowInputGrabbed = True
                                            windowInitialSize = fromIntegral <$> initialSize
                                            , windowOpenGL = Just defaultOpenGL { glProfile = Core Debug 3 3 }
                                            }
  c <- glCreateContext w
  glMakeCurrent w c

  giveContext $ do
    -- shaders
    vxsource <- T.readFile $ shaderPath <> ".vs"
    fgsource <- T.readFile $ shaderPath <> ".fs"
    pl <- handle (\(ShaderCompilationError msg) -> T.putStrLn msg >> fail "shader compilation error") $ newPipelineDebugVF vxsource fgsource M.empty

    svxsource <- T.readFile $ sshaderPath <> ".vs"
    sfgsource <- T.readFile $ sshaderPath <> ".fs"
    spl <- handle (\(ShaderCompilationError msg) -> T.putStrLn msg >> fail "shader compilation error") $ newPipelineDebugVF svxsource sfgsource M.empty

    cvxsource <- T.readFile $ "data/shaders/def_3dc" <> ".vs"
    cfgsource <- T.readFile $ "data/shaders/def_3dc" <> ".fs"
  
    cpl <- handle (\(ShaderCompilationError msg) -> T.putStrLn msg >> fail "shader compilation error") $ newPipelineDebugVF cvxsource cfgsource M.empty

    
    let pls = M.fromList [("default", pl), ("skinned", spl), ("colored", cpl)]
    
    -- .X files
    objd <- loadFromFile xDataTemplates "data/xobjects/" "lzom.x" False
    object <- initializeI objd

    sobjd <- loadFromFile xDataTemplates "data/xobjects/" "lzom.x" True
    skeleton <- loadFrameIX xDataTemplates "data/xobjects/lzomsk.x"
    sobject <- initializeS sobjd skeleton

    anim1 <- loadAnimation xDataTemplates "data/xobjects/lzomatk.X" skeleton
    print anim1
    -- map
    let
      posns = Prelude.map (+ (V2 (-1) (-1))) [V2 (-1) (-1), V2 (-1) 0, V2 (-1) 1, V2 (-1) 2, V2 (0) (-1), V2 (0) 0, V2 (0) 1, V2 (0) 2, V2 (1) (-1), V2 (1) 0, V2 (1) 1, V2 (1) 2, V2 (2) (-1), V2 (2) 0, V2 (2) 1, V2 (2) 2]
      posns' = Prelude.map (+ (V2 (-1) (-1))) [V2 0 0, V2 1 0, V2 0 1, V2 1 1]
    mBuff <- initMapBuffer "data/Textures/" ["sand1", "grass1", "grass3"]
    (gmp, mBuff) <- initChunks posns (initMap (NoiseRandom 1 1 0.05 0.5, NoiseRandom 1 1 0.05 0.5) [], mBuff)  --initChunkBuffers gmap posns' mBuff
    
    --light
    let light = DirectionalLight { lcolor = V3 0.3 0.3 0.3
                                 , ldirection = V3 (-0.42) (-0.57) (-0.71)
                                 , lambient = 0.7
                                 }
    fpsLimit <- newFPSLimit
 
    let initialState = GameInitialState {..}
        state0 = GameState { _camera = def
                           , _gmap = gmp
                           , _mapBuffer = mBuff
                           , _pressedKeys = S.empty
                           , _leftButton = False
                           , _frameSize = initialSize
                           , _frameTime = intervalTime
                           , _movedMouse = V2 0 0
                           }
    drawLoop w settings initialState state0

  glDeleteContext c
  destroyWindow w

-------------------------------------------------------------------------------------------------------
------------------------------------------ GAME LOOP --------------------------------------------------
-------------------------------------------------------------------------------------------------------
          
drawLoop :: Window -> GameSettings -> GameInitialState -> GameState -> IO ()
drawLoop w (GameSettings {..}) (GameInitialState {..}) = loop
  where
    loop st = do
      events <- pollEvents
      let mst = foldl processEvent (Just st { _movedMouse = V2 0 0 }) events
      case mst of
        Nothing -> return ()
        Just st' -> do
          let st'' = updateState st'
          st''' <- updateStateMonadic st''
          doDraw st'''
          ftime <- fpsDelay fpsLimit intervalTime
          loop (st''' & frameTime .~ ftime)

    processEvent Nothing _ = Nothing
    processEvent (Just st) (Event {..}) = case eventPayload of
      QuitEvent -> Nothing
      -- FIXME: update camera
      -- XXX: maybe WindowSizeChangedEvent is more proper here?
      WindowResizedEvent (WindowResizedEventData {..}) -> Just (st & frameSize .~ windowResizedEventSize)
      MouseMotionEvent (MouseMotionEventData {..}) | _leftButton st -> Just (st & movedMouse %~ (+ mouseMotionEventRelMotion))
      KeyboardEvent (KeyboardEventData {..}) -> case keyboardEventKeyMotion of
        Pressed -> Just (st & pressedKeys %~ S.insert (keysymKeycode keyboardEventKeysym))
        Released -> Just (st & pressedKeys %~ S.delete (keysymKeycode keyboardEventKeysym))
      MouseButtonEvent (MouseButtonEventData {..}) -> case mouseButtonEventMotion of
        Pressed -> Just (st & leftButton .~ True)
        Released -> Just (st & leftButton .~ False)
      _ -> Just st

    updateState = updatePos . updateLook
    updateStateMonadic = updateMap

    updatePos st@(GameState {..}) = let (V4 x y z _) = (viewMatrix _camera) !* (vector (movementSpeed * fromIntegral _frameTime *^ delta)) in st & camera %~ moveEye (V3 x y z) 
      where delta = normalize $ fwd + back + left + right + up + down

            moveKey k v
              | k `S.member` _pressedKeys = v
              | otherwise = V3 0 0 0

            fwd = moveKey KeycodeW (V3 0 0 (-1))
            back = moveKey KeycodeS (V3 0 0 1)
            left = moveKey KeycodeA (V3 (-1) 0 0)
            right = moveKey KeycodeD (V3 1 0 0)
            up = moveKey KeycodeSpace (V3 0 1 0)
            down = moveKey KeycodeLShift (V3 0 (-1) 0)
            

    updateLook st@(GameState {..}) =
      st & camera %~ rotateEyes (mouseSensitivity *^ V2 (-1) (-1) * (fromIntegral <$> _movedMouse) / (fromIntegral <$> _frameSize))

    -- TODO: clean the incorrect setters
    updateMap st@(GameState {..}) = do
      let (V2 x' y') = fmap (\x -> div x chunkWidth) (getHorizontal _camera)
          lst = map (+ (V2 x' y')) $ filter (\ (V2 x y) -> x^2 + y^2 <= mapLoadRadius^2) $ map (\ x -> V2 ((mod x (2*mapLoadRadius+1)) - mapLoadRadius) ((div x (2*mapLoadRadius+1)) - mapLoadRadius)) [1..(2*mapLoadRadius+1)^2]
      (gmap', mBuff') <- initChunks lst (_gmap, _mapBuffer) 
      return $ (st & gmap .~ gmap') & mapBuffer .~ mBuff'

    doDraw (GameState {..}) = do
      let mvM = viewMatrix _camera
          pM = projectionMatrix _camera

      -- print (_eye _camera, _rotation _camera)
      -- FIXME: set viewpoint size according to window size
      Car.clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer
      -- start drawing
      runDraws defaultDrawParams { pipeline = spl } $ do
        let 
        -- depth
        setFragmentPassTests defaultFragmentPassTests { depthTest = Just Less }

        -- pipeline initialization
        --pOffset <- Car.getUniformLocation "offsetMat[0]" $ spl
        --pBones <- Car.getUniformLocation "bonesMat[0]" $ spl
        pModelView <- Car.getUniformLocation "modelViewMat" $ spl
        pProjection <- Car.getUniformLocation "projectionMat" spl

        pTexture <- Car.getUniformLocation "tex" spl

        --let cspl = CPipeline{cPl = spl, cUniformsLoc = M.fromList [("offset", pOffset), ("bones", pBones), ("modelView", pModelView), ("texture", pTexture)]}

        -- set matrices
        setUniform pM pProjection spl

        -- lighting
        pLightCol <- Car.getUniformLocation "sunLight.color" spl
        pLightDir <- Car.getUniformLocation "sunLight.direction" spl
        pLightAmb <- Car.getUniformLocation "sunLight.ambient" spl

        setUniform (lcolor light) pLightCol spl
        setUniform (ldirection light) pLightDir spl
        setUniform (lambient light) pLightAmb spl
        -- meshes
        -- map
        --pProjectionC <- Car.getUniformLocation "projectionMat" cpl
        setUniform mvM pModelView spl
        drawChunks (MapContext spl pModelView mvM pTexture) _mapBuffer $ map fst (M.toList $ fst _mapBuffer)
        --        draw DContext {cpl = pl, cmvMLoc = mvMloc, ctexLoc = tloc, cmvM = mvM} object

        --drawS DContext {cpl = cspl, cmvM = mvM} sobject skeleton

      runPendingFinalizers
      glSwapWindow w
      where cpl = May.fromJust $ M.lookup "skinned" pls
            spl = May.fromJust $ M.lookup "colored" pls
