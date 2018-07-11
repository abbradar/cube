module Engine.Drawable
  ( ObjectD
  , Object
  , DContext(..)
  , DSContext(..)
  , loadFromFile
  , initializeI
  , initializeS
  , draw
  , drawS
  ) where

import Control.Monad

import Data.DirectX
import Data.Tree
import Data.Maybe
import Data.ByteString (ByteString)
import qualified Graphics.Caramia as C

import Engine.Types
import Engine.Mesh

import Debug.Trace

import Linear.Matrix
-- standart pipelines

--data ShaderTransformLoc = ShaderTransformLoc { sWorldLoc :: C.UniformLocation
--                                             , sModelWiewLoc :: C.UniformLocation
--                                             }
--data ShaderLightLoc = ShaderLightLoc { sLightColLoc :: C.UniformLocation
--                                     , sLightDirLoc :: C.UniformLocation
--                                     , slightAmbLoc :: C.UniformLocation
--                                     }

data CPipeline = CPipeline { cPl :: C.Pipeline
--                           , sTransformLoc :: [C.UniformLocation]
--                           , sTexLoc :: C.UniformLocation
--                           , sLightLoc :: [C.UniformLocation]
                           , cUniformsLoc :: [C.UniformLocation]
                           }

type Pipelines = [CPipeline]

--data Scene = Scene {
--                   }


data DContext = DContext { cpl :: C.Pipeline
--                       , cpl :: Pipelines
                         , cmvMLoc :: C.UniformLocation
                         , ctexLoc :: C.UniformLocation
                         , cmvM :: MF44
                         }

data DSContext = DSContext { cspl :: C.Pipeline
--                       , cpl :: Pipelines
                         , cBnesLocation :: C.UniformLocation
                         , cstexLoc :: C.UniformLocation
                         , cvM :: MF44
                         }

-- should contain any type of meshes, animations, various effects
data ObjectD = ObjectD { frames :: FrameTree
                         , filedir :: FilePath
                         } deriving (Show, Eq, Read)

data Object = Object { buffers :: FrameBufferTree
                     , bones :: Maybe FrameTree
                     }

-- OBSOLETE data SObject = SObject { sbuffers :: FrameBufferTree
--                       ,  bones :: Bones
--                       }

-- here there is a Bool variable, two paradigmas conflict, should fix it
loadFromFile :: XTemplates -> FilePath  -> FilePath -> Bool -> IO (ObjectD)
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
    obj <- mapM initSFBuffer (frames objd)
    print $ flatten skel
    return Object { buffers = obj, bones = Just skel }
    where
      --bns = generateSkeleton (frames objd)
      initSFBuffer = initSFrameBuffer (filedir objd) (mapMaybe (\x -> fname x) $ flatten skel)

-- FIX to a one function with a choice of a shader inside?

draw :: DContext -> Object -> C.DrawT IO ()
draw ctxt obj@(Object{bones = Nothing}) = do
--    setShader pl DEF_STATIC
    drawFrame (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

draw ctxt obj@(Object{bones = Just bns}) = undefined--do
--    SetShader pl DEF_SKINNED
          
--    drawFrameS (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

drawS :: DSContext -> Object -> FrameTree -> C.DrawT IO ()
drawS ctxt obj@(Object{bones = Just bnes}) skeleton = do
--    setShader pl DEF_STATIC
-- Offset matrices    
-- CAN RETURN -1 FIXME
    pOff <- C.getUniformLocation "offsetMat[0]" $ cspl ctxt 
    pTrans <- C.getUniformLocation "bonesMat[0]" $ cspl ctxt

--    forM_ (zip [0..] bones) (\(a,b) -> C.setUniform b (pTrans+a) (cspl ctxt))
-- FIXME make a list of uniforms for every element
--    setUniform (cBnesLocation ctxt) bones
    drawSFrame (buffers obj) bonesR (cstexLoc ctxt) pOff pTrans (cspl ctxt)
    where bonesR = genTransfMats (cvM ctxt)
          skelmats = fmap (ftransform) skeleton
          skelrec = (treeRecUpdate skelmats identity)
 --         treeRecUpdate :: (Tree MF44) -> MF44 -> (Tree MF44)
          treeRecUpdate (Node root children) mat = Node{rootLabel = (mat !*! root), subForest = (map (\x -> treeRecUpdate x (mat !*! root)) children)}
          -- FIXME needs to recursively compute the transform
          genTransfMats mat = map (\x -> (x !*! (cvM ctxt))) $ flatten skelrec

drawS ctxt obj@(Object{bones = Nothing}) _ = undefined--do
