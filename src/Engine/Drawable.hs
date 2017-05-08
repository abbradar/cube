module Engine.Drawable
  ( ObjectD
  , Object
  , SObject
  , DContext(..)
  , loadFromFile
  , initializeI
  , initializeS
  , drawI
  , drawS
  ) where

import Data.DirectX
import qualified Graphics.Caramia as C

import Engine.Types
import Engine.Mesh

import Debug.Trace

data DContext = DContext { cpl :: C.Pipeline
                         , cmvMLoc :: C.UniformLocation
                         , ctexLoc :: C.UniformLocation
                         , cmvM :: MF44
                         }

-- should contain any type of meshes, animations, various effects
data ObjectD = ObjectD { frames :: FrameTree
                         , filedir :: FilePath
                         } deriving (Show, Eq, Read)

data Object = Object { buffers :: FrameBufferTree
                       }

data SObject = SObject { sbuffers :: FrameBufferTree
                       ,  bones :: Bones
                       }

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
    return Object { buffers = obj }
    where
      initFBuffer = initIFrameBuffer (filedir objd)

initializeS :: ObjectD -> IO (SObject)
initializeS objd = do
    sobj <- mapM initSFBuffer (frames objd)
    return SObject { sbuffers = sobj, bones = bns }
    where
      bns = generateSkeleton (frames objd)
      initSFBuffer = initSFrameBuffer (filedir objd) (bns)

drawI :: DContext -> Object -> C.DrawT IO ()
drawI ctxt obj = drawFrame (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

drawS :: DContext -> SObject -> C.DrawT IO ()
drawS ctxt obj = drawFrame (sbuffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)
