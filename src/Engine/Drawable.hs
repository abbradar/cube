module Engine.Drawable
  ( ObjectD
  , Object
  , CPipeline(..)
  , CPipelines
  , Pipelines
  , DContext(..)
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
import Data.ByteString (ByteString, empty)
import qualified Graphics.Caramia as C
import qualified Data.Map.Strict as M

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
                           , cUniformsLoc :: M.Map ByteString C.UniformLocation
                           }

type CPipelines = M.Map ByteString CPipeline
type Pipelines = M.Map ByteString C.Pipeline

--data Scene = Scene {
--                   }


data DContext = DContext { cpl :: CPipeline
                         , cmvM :: MF44
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
 --         treeRecUpdate :: (Tree MF44) -> MF44 -> (Tree MF44)
      treeRecUpdate (Node root children) mat = Node{rootLabel = (root !*! mat), subForest = (map (\x -> treeRecUpdate x (root !*! mat)) children)}
          -- FIXME needs to recursively compute the transform
      genTransfMats = flatten skelrec
      


-- FIX to a one function with a choice of a shader inside?

draw :: DContext -> Object -> C.DrawT IO ()
draw _ _ = undefined
--draw ctxt obj@(Object{bones = Nothing}) = do
--    drawFrame (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

--draw ctxt obj@(Object{bones = Just bns}) = undefined--do
--    SetShader pl DEF_SKINNED
          
--    drawFrameS (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

drawS :: DContext -> Object -> FrameTree -> C.DrawT IO ()
drawS ctxt obj@(Object{bones = Just bnes}) skeleton = do
-- CAN RETURN -1 FIXME
--  fromJust, FIXME 
    C.setUniform (cmvM ctxt) pModelView pl

    drawSFrame (buffers obj) bonesR pTexture pOffset pBones pl

    where plData = (cpl ctxt)
          pl = cPl $ plData
          pOffset = fromJust $ M.lookup "offset" (cUniformsLoc plData)
          pBones = fromJust $ M.lookup "bones" (cUniformsLoc plData)
          pModelView = fromJust $ M.lookup "modelView" (cUniformsLoc plData)
          pTexture = fromJust $ M.lookup "texture" (cUniformsLoc plData)
    
          bonesR = genTransfMats
          skelmats = fmap (ftransform) skeleton
          skelrec = (treeRecUpdate skelmats identity)
 --         treeRecUpdate :: (Tree MF44) -> MF44 -> (Tree MF44)
          treeRecUpdate (Node root children) mat = Node{rootLabel = (root !*! mat), subForest = (map (\x -> treeRecUpdate x (root !*! mat)) children)}
          -- FIXME needs to recursively compute the transform
          genTransfMats = flatten skelrec

drawS ctxt obj@(Object{bones = Nothing}) _ = undefined--do
