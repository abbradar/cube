module Engine.Drawable
  ( ObjectD
  , Object
  , DContext(..)
  , loadFromFile
  , initializeI
  , initializeS
  , draw
  ) where

import Data.DirectX
import qualified Graphics.Caramia as C

import Engine.Types
import Engine.Mesh

import Debug.Trace

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

-- should contain any type of meshes, animations, various effects
data ObjectD = ObjectD { frames :: FrameTree
                         , filedir :: FilePath
                         } deriving (Show, Eq, Read)

data Object = Object { buffers :: FrameBufferTree
                     , bones :: Maybe Bones
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

initializeS :: ObjectD -> IO (Object)
initializeS objd = do
    obj <- mapM initSFBuffer (frames objd)
    print $ bns
    return Object { buffers = obj, bones = Just bns }
    where
      bns = generateSkeleton (frames objd)
      initSFBuffer = initSFrameBuffer (filedir objd) (bns)

draw :: DContext -> Object -> C.DrawT IO ()
draw ctxt obj@(Object{bones = Nothing}) = do
--    setShader pl DEF_STATIC
    drawFrame (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)

draw ctxt obj@(Object{bones = Just bns}) = undefined--do
--    SetShader pl DEF_SKINNED
          
--    drawFrameS (buffers obj) (cmvMLoc ctxt) (ctexLoc ctxt) (cmvM ctxt) (cpl ctxt)
