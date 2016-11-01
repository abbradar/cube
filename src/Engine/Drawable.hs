module Engine.Drawable
  ( XObjectD
  , XObject
  , DContext(..)
  , loadFromFile
  , initialize
  , draw
  ) where

import Data.DirectX
import qualified SDL as S
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
data XObjectD = XObjectD { xframes :: FrameTree
                         , filedir :: FilePath
                         } deriving (Show, Eq, Read)

data XObject = XObject { xbuffers :: FrameBufferTree
                       }

loadFromFile :: XTemplates -> FilePath -> FilePath -> IO (XObjectD)
loadFromFile tmpls fdir fname = do
    xfs <- loadFrameX tmpls (fdir ++ fname)
    return XObjectD { xframes = xfs, filedir = fdir }

initialize :: XObjectD -> IO (XObject)
initialize objd = do
    obj <- mapM initFBuffer (xframes objd)
    return XObject { xbuffers = obj }
  where
    initFBuffer = initFrameBuffer (filedir objd)

draw :: DContext -> XObject -> C.DrawT IO ()
draw ctxt obj = drawFrame (xbuffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)
