module Engine.Drawable
  ( ObjectD
  , Object
  , CPipeline(..)
  , CPipelines
  , Pipelines
  , DContext(..)
  , MapContext(..)
  , AnimObjData(..)
  , loadFromFile
  , initializeI
  , initializeS
  , draw
  , updateSkeleton
  , drawS
  , drawAnimated
  , drawChunks
  , drawChunk
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Tree
import Data.Maybe
import Data.ByteString (ByteString, empty)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Graphics.Caramia as C
import qualified Data.Vector.Storable as VS
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix

import Engine.Types
import Engine.Mesh
import Engine.Loaders
import Engine.Map
import Engine.Chunk

import Debug.Trace

-- standard pipelines

--data ShaderTransformLoc = ShaderTransformLoc { sWorldLoc :: C.UniformLocation
--                                             , sModelWiewLoc :: C.UniformLocation
--                                             }
--data ShaderLightLoc = ShaderLightLoc { sLightColLoc :: C.UniformLocation
--                                     , sLightDirLoc :: C.UniformLocation
--                                     , slightAmbLoc :: C.UniformLocation
--                                     }

data CPipeline = CPipeline { cPl :: C.Pipeline
                           , cUniformsLoc :: M.Map ByteString C.UniformLocation
                           }

type CPipelines = Map ByteString CPipeline
type Pipelines = Map ByteString C.Pipeline

--data Scene = Scene {
--                   }


data DContext = DContext { cpl :: CPipeline
                         , cmvM :: MF44
                         }

-- should contain any type of meshes, animations, various effects
data ObjectD = ObjectD { frames :: FrameTree
                       , filedir :: FilePath
                       }
             deriving (Show, Eq, Read)

data Object = Object { buffers :: FrameBufferTree
                     , bones :: Maybe FrameTree
                     }

data AnimObjData = AnimObjData { _object :: Object
                               , _skeleton :: FrameTree
                               , _anims :: M.Map ByteString (Float, [Animation])
                               }


-- OBSOLETE data SObject = SObject { sbuffers :: FrameBufferTree
--                       ,  bones :: Bones
--                       }

-- here there is a Bool variable, two paradigmas conflict, should fix it
loadFromFile :: XTemplates -> FilePath -> FilePath -> Bool -> IO (ObjectD)
loadFromFile tmpls fdir fname isskinned = do
    fs <- ldFrameX tmpls (fdir ++ fname)
    return ObjectD { frames = fs, filedir = fdir }
    where
      ldFrameX = if isskinned then loadFrameSX else loadFrameIX

initializeI :: ObjectD -> IO (Object)
initializeI objd = do
    obj <- mapM initFBuffer (frames objd)
    return Object { buffers = obj, bones = Nothing }
    where
      initFBuffer = initIFrameBuffer (filedir objd)

initializeS :: ObjectD -> FrameTree -> IO (Object)
initializeS objd skel = do
    let initSFBRec (Node rt chldren) mat = Node{rootLabel = initSFBuffer rt mat, subForest = (map (\x -> initSFBRec x ((ftransform rt) !*! mat)) chldren)}
    obj <- sequence $ initSFBRec (frames objd) identity
    --   print $ flatten skel
    return Object { buffers = obj, bones = Just skel }
    where
      --bns = generateSkeleton (frames objd)
      initSFBuffer x mat = initSFrameBuffer (filedir objd) (map (\x -> getName $ fname x) $ flatten skel) bonesR mat x
      getName (Just nm) = nm
      getName Nothing = empty
      -- Computing offsetMatrices
      bonesR = genTransfMats
      skelmats = fmap (ftransform) skel
      skelrec = (treeRecUpdate skelmats identity)
      -- treeRecUpdate :: (Tree MF44) -> MF44 -> (Tree MF44)
      treeRecUpdate (Node root children) mat = Node{rootLabel = (root !*! mat), subForest = (map (\x -> treeRecUpdate x (root !*! mat)) children)}
      -- FIXME needs to recursively compute the transform
      genTransfMats = flatten skelrec
      


-- FIX to a one function with a choice of a shader inside?

draw :: DContext -> Object -> C.DrawT IO ()

draw ctxt obj@(Object{bones = Nothing}) = do
    drawFrame (buffers obj) pModelView pTexture (cmvM ctxt) (cPl plData)
    where
      plData = cpl ctxt
      checkPositive a = if a >= 0 then a else error "Uniform not found"
      pModelView = checkPositive $ fromJust $ M.lookup "modelView" (cUniformsLoc plData)
      pTexture = checkPositive $ fromJust $ M.lookup "texture" (cUniformsLoc plData)
      
draw _ _ = undefined

--draw ctxt obj@(Object{bones = Just bns}) = undefined--do
--    SetShader pl DEF_SKINNED


-- updates frame matrices given a list of animations, time scale is [0,1]
-- no interpolation so far
updateSkeleton :: FrameTree -> [Animation] -> Float -> FrameTree
updateSkeleton skeleton [] _ = skeleton
updateSkeleton skeleton ((name, transforms):s) time = updateSkeleton skeleton' s time
  where
    matnumber :: Int
    matnumber = floor ((fromIntegral $ VS.length transforms)*(snd $ properFraction time))
    transform = transforms VS.! matnumber
    changeFrameTransform Frame{ fmesh = m, fname = n,  tname = t, ftransform = tr } mat = Frame{ fmesh = m, fname = n,  tname = t, ftransform = mat}
    skeleton' = animTreeRecUpdate skeleton
    animTreeRecUpdate (Node root children)
      | (fname root) == (Just name) = Node{rootLabel = (changeFrameTransform root transform), subForest = (map animTreeRecUpdate children)}
      | otherwise = Node{rootLabel = root, subForest = (map animTreeRecUpdate children)}

           
--    drawFrameS (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)



drawAnimated :: DContext -> AnimObjData -> ByteString -> Float -> C.DrawT IO ()
drawAnimated ctxt dt@(AnimObjData{..}) animName animTime = do
  drawS ctxt _object (updateSkeleton _skeleton (snd anim) $ (fst anim)*animTime)
  where
    anim = fromMaybe (error "no required animation found") $ M.lookup animName _anims

drawS :: DContext -> Object -> FrameTree -> C.DrawT IO ()
drawS ctxt obj@(Object{bones = Just bnes}) skeleton = do
-- CAN RETURN -1 FIXME
--  fromJust, FIXME
    C.setUniform (cmvM ctxt) pModelView pl

    drawSFrame (buffers obj) bonesR pTexture pOffset pBones pl
    where plData = (cpl ctxt)
          pl = cPl $ plData
          checkPositive a = if a >= 0 then a else error "Uniform not found"
          pOffset = checkPositive $ fromJust $ M.lookup "offset" (cUniformsLoc plData)
          pBones = checkPositive $ fromJust $ M.lookup "bones" (cUniformsLoc plData)
          pModelView = checkPositive $ fromJust $ M.lookup "modelView" (cUniformsLoc plData)
          pTexture = checkPositive $ fromJust $ M.lookup "texture" (cUniformsLoc plData)
    
          bonesR = genTransfMats
          genTransfMats = flatten skelrec
          skelrec = (treeRecUpdate skelmats identity)
          -- treeRecUpdate :: (Tree MF44) -> MF44 -> (Tree MF44)
          treeRecUpdate (Node root children) mat = Node{rootLabel = (root !*! mat), subForest = (map (\x -> treeRecUpdate x (root !*! mat)) children)}
          -- FIXME needs to recursively compute the transform
          skelmats = fmap (ftransform) skeleton

drawS ctxt obj@(Object{bones = Nothing}) _ = undefined--do


data MapContext = MapContext { mPl :: C.Pipeline
                             , mTrLoc :: C.UniformLocation
                             , mView :: MF44
                             , pTex :: C.UniformLocation
                             }

drawChunks :: MapContext -> MapBuffer -> [HorizontalPos] -> C.DrawT IO ()
drawChunks _ _ [] = return ()
drawChunks ctxt buffs (x:xs) = do
  drawChunk ctxt buffs x
  drawChunks ctxt buffs xs
  
-- TODO: do somthing with this
defTex' :: Int
defTex' = 0

  
drawChunk :: MapContext -> MapBuffer -> HorizontalPos -> C.DrawT IO ()
drawChunk (MapContext {..}) (cBuffs, texs) pos@(V2 x y) =
  case M.lookup pos cBuffs of
    Nothing -> return ()
    Just buff -> do
-- position uniform
      C.setUniform (shiftMat !*! mView) mTrLoc mPl
      mapM_ drawChunkSubset buff'
      where
        -- todo correct this mess
        buff' = map (\ (x'', y'') -> (fromMaybe (error "Empty map buffers") x'', catMaybes (map (\ z -> M.lookup z texs) y'')))  buff
        shiftMat = transpose $ (mkTransformationMat identity (V3 x' y' 0.0))
        x' :: Float
        x' = fromIntegral (chunkWidth*x)
        y' = fromIntegral (chunkWidth*y)
        drawChunkSubset (buff'', csTexs') = do
          -- texture uniforms
          -- TODO: normal texturing
          if(length csTexs' > 0) then C.setTextureBindings (IM.singleton 0 (csTexs' !! 0)) else error "No texture found for the chunk"
          C.setUniform defTex' pTex mPl
          drawMesh buff'' mPl

          
