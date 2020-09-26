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
import qualified Data.Tree as DT

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
                                         , anims :: [(Float, [Animation])]
                                        -- , gmap :: Map
                                        -- , mapBuffer :: MapBuffer
                                         , skeleton :: FrameTree
                                         , light :: DirectionalLight
                                         , fpsLimit :: FPSLimit
                                         , xDataTemplates :: XTemplates
                                         }

data PlayerState = Idle | Running | Attack Word32 deriving (Eq,Ord,Show)

data GameState = GameState { _camera :: Camera
                           , _gmap :: Map
                           , _playerPos :: F3
                           , _playerState :: PlayerState
                           , _mapBuffer :: MapBuffer
                           , _leftButton :: Bool
                           , _pressedKeys :: Set Keycode
                           , _movedMouse :: V2 Int32
                           , _frameSize :: V2 Int32
                           , _frameTime :: Word32
                           , _currentTime :: Word32
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

  w <- createWindow "A Window" defaultWindow { windowInputGrabbed = True
                                             ,  windowInitialSize = fromIntegral <$> initialSize
                                             , windowGraphicsContext = OpenGLContext defaultOpenGL { glProfile = Core Debug 3 3 }
                                             }

  setMouseLocationMode RelativeLocation
       
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

    sobjd <- loadFromFile xDataTemplates "data/xobjects/" "human1.x" True
    skeleton <- loadFrameIX xDataTemplates "data/xobjects/human1.x"
    print $ map fname (DT.flatten skeleton)

    sobject <- initializeS sobjd skeleton

    animRun <- loadAnimation xDataTemplates "data/xobjects/humanRun.x" skeleton
    animIdle <- loadAnimation xDataTemplates "data/xobjects/humanIdle.x" skeleton
    animAtk <- loadAnimation xDataTemplates "data/xobjects/humanAtk.x" skeleton
  
    let anims = [(1.0, animRun), (0.5, animIdle), (1, animAtk)]
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
    tme <- getTicks
 
    let initialState = GameInitialState {..}
        state0 = GameState { _camera = def
                           , _gmap = gmp
                           , _playerPos = V3 0 0 16
                           , _playerState = Idle
                           , _mapBuffer = mBuff
                           , _pressedKeys = S.empty
                           , _leftButton = False
                           , _frameSize = initialSize
                           , _frameTime = intervalTime
                           , _currentTime = tme
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
          tme <-getTicks
          ftime <- fpsDelay fpsLimit intervalTime
          loop (st''' & frameTime .~ ftime & currentTime .~ tme)

    processEvent Nothing _ = Nothing
    processEvent (Just st) (Event {..}) = case eventPayload of
      QuitEvent -> Nothing
      -- FIXME: update camera
      -- XXX: maybe WindowSizeChangedEvent is more proper here?
      WindowResizedEvent (WindowResizedEventData {..}) -> Just (st & frameSize .~ windowResizedEventSize)
      MouseMotionEvent (MouseMotionEventData {..}) -> Just (st & movedMouse %~ (+ mouseMotionEventRelMotion))
      KeyboardEvent (KeyboardEventData {..}) -> case keyboardEventKeyMotion of
        Pressed -> if KeycodeEscape == (keysymKeycode keyboardEventKeysym) then Nothing else Just (st & pressedKeys %~ S.insert (keysymKeycode keyboardEventKeysym))
        Released -> Just (st & pressedKeys %~ S.delete (keysymKeycode keyboardEventKeysym))
      MouseButtonEvent (MouseButtonEventData {..}) -> case mouseButtonEventMotion of
        Pressed -> Just (st & leftButton .~ True)
        Released -> Just (st & leftButton .~ False)
      _ -> Just st

    updateState = updatePlayer . updateLook

    updateStateMonadic = updateMap


    updatePlayer st@(GameState {..}) = st & playerState %~ state & playerPos .~ (V3 (x+dx1) (y+dy1) (fromIntegral z1 - 16)) & camera %~ setEye (V3 (x+dx1) (y+dy1) (fromIntegral z1 - 16+1))
      --let (V4 x y z _) = (viewMatrix _camera) !* (vector (movementSpeed * fromIntegral _frameTime *^ delta)) in st & camera %~ moveEye (V3 x y z) 
      where (V3 x y z) = _eye _camera 
            (V3 dx dy _) = if ((state _playerState) == Running) then (movementSpeed * fromIntegral _frameTime *^ delta) else V3 0.0 0.0 0.0
            delta = normalize $ fwd + back + left + right -- + up + down

            (V3 x' y' z') = delta

            -- check current state of the player
            state :: PlayerState -> PlayerState
            state (Attack time') = if _leftButton || (_currentTime - time' < 1000) then updateAttack time' else
                                     if (nearZero x') && (nearZero y') then Idle else Running
            state _ = if _leftButton then (Attack _currentTime) else
                        if (nearZero x') && (nearZero y') then Idle else Running

            updateAttack time' = if (_currentTime - time' < 1000) then (Attack time') else Attack _currentTime
                        
            dx1 = dx * (cos phi) - dy * (sin phi)
            dy1 = dx * (sin phi) + dy * (cos phi)
            (V2 phi _) = _angles _camera

            (V3 _ _ z1) = getClosestGround chck $ (fromIntegral <$> (V3 0 0 31)) + ((\x -> mod x chunkWidth) <$> floor <$> (V3 (x+dx1) (y+dy1) 0))
            chck = May.fromMaybe (error "chunk with the player is not initialized") $ getChunk _gmap ((\x -> div x chunkWidth) <$> (floor <$> (V2 (x+dx1) (y+dy1))))
              
            moveKey k v
              | k `S.member` _pressedKeys = v
              | otherwise = V3 0 0 0

            fwd = moveKey KeycodeW (V3 1 0 0)
            back = moveKey KeycodeS (V3 (-1) 0 0)
            left = moveKey KeycodeA (V3 0 1 0)
            right = moveKey KeycodeD (V3 0 (-1) 0)
            --up = moveKey KeycodeSpace (V3 0 1 0)
           -- down = moveKey KeycodeLShift (V3 0 (-1) 0)
            

    updateLook st@(GameState {..}) =
      st & camera %~ rotateEyes (mouseSensitivity *^ V2 (-1) (-1) * (fromIntegral <$> _movedMouse) / (fromIntegral <$> _frameSize))

    -- TODO: clean the incorrect setters
    updateMap st@(GameState {..}) = do
      let (V2 x' y') = fmap (\x -> div x chunkWidth) (getHorizontal _camera)
          lst = map (+ (V2 x' y')) $ filter (\ (V2 x y) -> x^2 + y^2 <= mapLoadRadius^2) $ map (\ x -> V2 ((mod x (2*mapLoadRadius+1)) - mapLoadRadius) ((div x (2*mapLoadRadius+1)) - mapLoadRadius)) [1..(2*mapLoadRadius+1)^2]
      (gmap', mBuff') <- initChunks lst (_gmap, _mapBuffer) 
      return $ st & gmap .~ gmap' & mapBuffer .~ mBuff'


    doDraw (GameState {..}) = do
      let mvM = viewMatrixLookAt _camera 4.0
          pM = projectionMatrix _camera

      --print (_eye _camera, _rotation _camera)
      -- FIXME: set viewpoint size according to window size
      Car.clear clearing { clearDepth = Just 1.0
                         , clearColor = Just $ rgba 0.4 0.4 0.4 1.0
                         } screenFramebuffer

      -- start unskinned draw
  --    runDraws defaultDrawParams { pipeline = cpl } $ do
  --      let 
        -- depth
  --      setFragmentPassTests defaultFragmentPassTests { depthTest = Just Less }
  --      pModelViewI <- Car.getUniformLocation "modelViewMat" $ cpl
  --      pTextureI <- Car.getUniformLocation "tex" cpl
  --      pProjectionI <- Car.getUniformLocation "projectionMat" cpl
       
        -- pipeline wrapper for NON-skinned objects = pipeline + uniform locations
  --      let ccpl = CPipeline{cPl = cpl, cUniformsLoc = M.fromList [("modelView", pModelViewI), ("texture", pTextureI)]}

  --      setUniform pM pProjectionI cpl
  --      setUniform mvM pModelViewI cpl
        
  --      draw DContext {cpl = ccpl, cmvM = mvM} object


      runDraws defaultDrawParams { pipeline = cpl } $ do
        
      -- depth
        setFragmentPassTests defaultFragmentPassTests { depthTest = Just Less }

        -- pipeline initialization
        

        pModelView <- Car.getUniformLocation "modelViewMat" $ cpl
        pProjection <- Car.getUniformLocation "projectionMat" cpl
        pTexture <- Car.getUniformLocation "tex" cpl

        -- lighting
        pLightCol <- Car.getUniformLocation "sunLight.color" cpl
        pLightDir <- Car.getUniformLocation "sunLight.direction" cpl
        pLightAmb <- Car.getUniformLocation "sunLight.ambient" cpl

        setUniform (lcolor light) pLightCol cpl
        setUniform (ldirection light) pLightDir cpl
        setUniform (lambient light) pLightAmb cpl
        
        -- set matrices
        setUniform pM pProjection cpl

        drawChunks (MapContext cpl pModelView mvM pTexture) _mapBuffer $ map fst (M.toList $ fst _mapBuffer)
              --  draw DContext {cpl = pl, cmvMLoc = mvMloc, ctexLoc = tloc, cmvM = mvM} object

        
      -- start drawing skinned
      runDraws defaultDrawParams { pipeline = spl } $ do
        let 
        -- depth
        setFragmentPassTests defaultFragmentPassTests { depthTest = Just Less }

        -- pipeline initialization
        pOffset <- Car.getUniformLocation "offsetMat[0]" $ spl
        pBones <- Car.getUniformLocation "bonesMat[0]" $ spl

        pModelView <- Car.getUniformLocation "modelViewMat" $ spl
        pProjection <- Car.getUniformLocation "projectionMat" spl

        pTexture <- Car.getUniformLocation "tex" spl
        

        -- pipeline wrapper for skinned objects = pipeline + uniform locations
        let cspl = CPipeline{cPl = spl, cUniformsLoc = M.fromList [("offset", pOffset), ("bones", pBones), ("modelView", pModelView), ("texture", pTexture)]}

        -- set matrices
        setUniform pM pProjection spl

        -- lighting
        pLightCol <- Car.getUniformLocation "sunLight.color" spl
        pLightDir <- Car.getUniformLocation "sunLight.direction" spl
        pLightAmb <- Car.getUniformLocation "sunLight.ambient" spl

        setUniform (lcolor light) pLightCol spl
        setUniform (ldirection light) pLightDir spl
        setUniform (lambient light) pLightAmb spl
        
        --setUniform ((viewMatrix _camera) !*! mvM) pModelView spl
   
        -- skinned model render

        let modelMatrix = scaleMatrix (V3 0.3 0.3 0.3) $ transpose $ let (V2 phi _) = (_angles _camera) in mkTransformation (Quaternion (cos $ (phi+pi/2)/2) (V3 0 0 (sin $ (phi+pi/2)/2))) (_playerPos)
                    -- (inv44 (viewMatrixAngle _camera 0))                                                             
        let (animNumber, tme') = case _playerState of
                                   Running -> (0, _currentTime)
                                   Idle -> (1, _currentTime)
                                   Attack tme'' -> (2, _currentTime - tme'')
        drawS DContext {cpl = cspl, cmvM = ( modelMatrix !*! mvM)} sobject (updateSkeleton skeleton (snd (anims !! animNumber)) ((fst (anims !! animNumber))*(fromIntegral tme')/1000.0))
        --drawS DContext {cpl = cspl, cmvM = mvM} sobject skeleton
        
      runPendingFinalizers
      glSwapWindow w
      where cpl = May.fromJust $ M.lookup "colored" pls
            spl = May.fromJust $ M.lookup "skinned" pls
