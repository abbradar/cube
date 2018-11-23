module Engine.Mesh
       ( Vert
       , Ind
       , Mesh(..)
       , IMesh(..)
       , SMesh(..)
       , Frame(..)
       , FrameBuffer
       , Bone
       , Bones
       , SkinData(..)
       , Animation
       , AnimationSet
       , generateSkeleton
       , MeshBuffer
       , initMeshBuffer
       , drawMesh
       , initIFrameBuffer
       , initSFrameBuffer
       , drawFrame
       , drawSFrame
       , FrameTree
       , FrameBufferTree
       ) where

-- import qualified Data.Map as M
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Graphics.Caramia
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Foreign.Storable.Tuple ()

import Data.Tree (Tree(..))
import Engine.Types

import Debug.Trace



-- skin data for loading
data SkinData = SkinData { sdvertices :: [(V4 Word8, V4 Float)]
                         , sdbones :: [(ByteString, MF44)]
                         }
             deriving (Generic, Show, Eq, Read)




-- bone for skeleton (name, dynamic matrix)
-- TODO: make an array for fast access and mutability
type Bone = (ByteString, MF44)
type Bones = [Bone]

-- mesh bone info: (id, static matrix)
-- TODO: make an array for fast access
type BonesInfo = [(Int, MF44)]

type Animation = (Float, Float -> FrameTree, [FrameTree])

data AnimationSet = AnimationSet { anumber :: Int
                                 , animations :: [Animation]
                                 , currAnum :: Animation
                                 , currTime :: Float
                                 }

--  "I(nanimate)Foo - S(kinned)Foo - Foo = (IF IFoo | SF SFoo) "

-- mesh for static textured objects
data IMesh = IMesh { vertices :: Vector VertexD
                 , indices :: Vector Ind
                 }
          deriving (Show, Eq, Read)

-- mesh for skinned objects
data SMesh = SMesh { svertices :: Vector SVertexD
                   , sindices :: Vector Ind
                   , sbones :: Bones
                   }
           deriving (Show, Eq, Read)
-- general mesh, D for Default
data Mesh = IM IMesh | SM SMesh
           deriving (Show, Eq, Read)

-- frame (contains any type of mesh). According to our logic all 
-- the data about mesh except vertices is stored in frame (buffer)
-- for instance texture, shader info, bones info (if present).
-- Also one frame may contain only one mesh. 
data Frame = Frame { fmesh :: Maybe Mesh
                   , fname :: Maybe ByteString
                   , tname :: Maybe ByteString
                   , ftransform :: MF44
                   }
           deriving (Show, Eq, Read)

-- buffer for mesh
data MeshBuffer = MeshBuffer { mvao :: VAO
                             , mbuffer :: Buffer
                             , indOffset :: Int
                             , indNumber :: Int
                             } 

instance Show MeshBuffer where
  show buf = "MeshBuffer"

-- TODO: remove unnecessary Frame information
data FrameBuffer = FrameBuffer {fframe :: Frame
                                 , fbuffer :: Maybe MeshBuffer
                                 , fmeshbonesinfo :: Maybe BonesInfo
                                 , ftexture :: Maybe Texture
                                 }
--  "I(nanimate)Foo - S(kinned)Foo - Foo = (IF IFoo | SF SFoo) "
-- OBSOLETE data FrameBuffer = IB IFrameBuffer | SB SFrameBuffer

-- trees for frames
type FrameTree = Tree Frame
type FrameBufferTree = Tree FrameBuffer

---------------------------------------------------------------------------
---------------------------- INITIALIZATION -------------------------------
---------------------------------------------------------------------------

-- FIXMI (nonfinished)
generateSkeleton :: FrameTree -> [ByteString]
generateSkeleton _ = undefined
--generateSkeleton frames = filter (\x -> elem (fname x) bnes) frames
--			  where
--				bnes = concat $ map getbones (toList frames)
--                            	getbones (Frame {fmesh =  Nothing}) = []
--                            	getbones (Frame {fmesh = Just (IM a)}) = []
--                           	getbones (Frame {fmesh = Just (SM a)}) = sbones a




initMeshBuffer :: Mesh -> IO MeshBuffer
initMeshBuffer (IM mesh) = initMeshBufferD mesh
initMeshBuffer (SM smesh) = initMeshBufferDS smesh



initMeshBufferD :: IMesh -> IO MeshBuffer
initMeshBufferD mesh = do
  vao <- newVAO
  buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                          , size = sizeOfV (vertices mesh) + sizeOfV (indices mesh)
                                          }
  uploadVector (vertices mesh) 0 buff
  sourceVertexData buff defaultSourcing { components = 3
                                        , stride = sizeOf (undefined :: VertexD)
                                        , attributeIndex = 0
                                        , sourceType = SFloat
                                        } vao
  
  sourceVertexData buff defaultSourcing { offset = sizeOf (undefined :: (V3 Float))
                                        , components = 3
                                        , stride = sizeOf (undefined :: VertexD)
                                        , attributeIndex = 1
                                        , sourceType = SFloat
                                        } vao
  sourceVertexData buff defaultSourcing { offset = 2 * sizeOf (undefined :: (V3 Float))
                                        , components = 2
                                        , stride = sizeOf (undefined :: VertexD)
                                        , attributeIndex = 2
                                        , sourceType = SFloat
                                        } vao

  uploadVector (indices mesh) (sizeOfV (vertices mesh)) buff
  return MeshBuffer { mvao = vao
                    , mbuffer = buff
                    , indOffset = sizeOfV (vertices mesh)
                    , indNumber = 3 * VS.length (indices mesh)
                    }

initMeshBufferDS :: SMesh -> IO MeshBuffer
initMeshBufferDS smesh = do
  vao <- newVAO
  buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                          , size = sizeOfV (svertices smesh) + sizeOfV (sindices smesh)
                                          }
  uploadVector (svertices smesh) 0 buff
  sourceVertexData buff defaultSourcing { components = 3
                                        , stride = sizeOf (undefined :: SVertexD)
                                        , attributeIndex = 0
                                        , sourceType = SFloat
                                        } vao
  
  sourceVertexData buff defaultSourcing { offset = sizeOf (undefined :: (V3 Float))
                                        , components = 3
                                        , stride = sizeOf (undefined :: SVertexD)
                                        , attributeIndex = 1
                                        , sourceType = SFloat
                                        } vao
  sourceVertexData buff defaultSourcing { offset = 2 * sizeOf (undefined :: (V3 Float))
                                        , components = 2
                                        , stride = sizeOf (undefined :: SVertexD)
                                        , attributeIndex = 2
                                        , sourceType = SFloat
                                        } vao
  sourceVertexData buff defaultSourcing { offset = 2 * sizeOf (undefined :: (V3 Float)) + sizeOf (undefined :: (V2 Float) )
                                        , components = 4
                                        , stride = sizeOf (undefined :: SVertexD)
                                        , attributeIndex = 3
                                        , sourceType = SWord8
                                        , integerMapping = True
                                        } vao
  sourceVertexData buff defaultSourcing { offset = 2 * sizeOf (undefined :: (V3 Float)) + sizeOf (undefined :: (V2 Float))  + sizeOf (undefined :: (V4 Word8))
                                        , components = 4
                                        , stride = sizeOf (undefined :: SVertexD)
                                        , attributeIndex = 4
                                        , sourceType = SFloat
                                        } vao



  uploadVector (sindices smesh) (sizeOfV (svertices smesh)) buff
  return MeshBuffer { mvao = vao
                    , mbuffer = buff
                    , indOffset = sizeOfV (svertices smesh)
                    , indNumber = 3 * VS.length (sindices smesh)
                    }





loadTex :: FilePath -> IO (Maybe Texture)
loadTex file = do
    imgl <- readImage file
    case imgl of 
      Right img -> Just <$> texture img
      _ -> return Nothing


texture :: DynamicImage -> IO Texture
texture (ImageRGB8 image@(Image w h _)) = do
  tx <- newTexture spec
  buff <- newBufferFromVector (imageData image) id
  uploadToTexture (uploading2D buff w h FWord8 URGB) tx
  return tx

  where
    spec = textureSpecification { topology = Tex2D { width2D = w, height2D = h }
                                , imageFormat = RGB8
                                }

texture _  = fail "format not available"

--loadAnimation :: Data -> Animation
--loadAnimation dt = (lgth, animState, keys)
--  where keys = genSkel

-- initializes frame buffer. fdir is for the texture.
-- Maybe we should put it somwhere else

initIFrameBuffer :: FilePath -> Frame -> IO FrameBuffer
initIFrameBuffer fdir frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  let fname = fmap B.unpack (tname frame)
      name' = fmap (fdir ++ ) fname
  tex <- maybe (return Nothing) loadTex name'
  return $ FrameBuffer { fframe = frame, fbuffer = mbuf, fmeshbonesinfo = Nothing, ftexture = tex }

initSFrameBuffer :: FilePath -> [ByteString] -> [MF44] -> MF44 -> Frame -> IO FrameBuffer
initSFrameBuffer fdir bs bsm trans frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  let fname = fmap B.unpack (tname frame)
      name' = fmap (fdir ++ ) fname
      binfo = genBones bs bsm (fmesh frame)
  tex <- maybe (return Nothing) loadTex name'
  return $ FrameBuffer { fframe = frame, fbuffer = mbuf, fmeshbonesinfo = Just binfo, ftexture = tex }
  where 
    genBones :: [ByteString] -> [MF44] -> Maybe Mesh -> BonesInfo 
    genBones _ _ Nothing = []
    genBones _ _ (Just (IM _)) = []
 -- FROMJUST OLOLO FIXMI
    genBones bones bmats (Just (SM smesh)) = map (\(name, mx) -> (fromJust (findIndex (== name) bones), (ftransform frame) !*! trans !*! inv44 (bmats !! fromJust (findIndex (== name) bones)))) $ sbones smesh
      

defTex' :: Int
defTex' = 0

drawMesh :: MeshBuffer -> Pipeline -> DrawT IO ()
drawMesh mesh pl =
  drawR drawCommand { primitiveType = Triangles
                    , primitivesVAO = mvao mesh
                    , numIndices = indNumber mesh
                    , sourceData = PrimitivesWithIndices (mbuffer mesh) (indOffset mesh) IWord16
                    }
---------------------------------------------------------------------------
------------------------------ DRAWING ------------------------------------
---------------------------------------------------------------------------

drawFrame :: Tree FrameBuffer -> UniformLocation -> UniformLocation -> MF44 -> Pipeline -> DrawT IO ()

drawFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Just skel})) fbchildren) mloc tloc mvM pl = error "use drawSFrame for skinned mesh"


drawFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Nothing})) fbchildren) mloc tloc mvM pl = do
-- plugs in a transform matrix location in pl shader
  setUniform nmvM mloc pl
-- binds a texture in pl shader
  case ftexture fbuf of
    Just tex' -> do 
      setTextureBindings (IM.singleton defTex' tex')
      setUniform defTex' tloc pl
    Nothing -> return ()
-- draws a mesh on a pl
  unless (isNothing (fbuffer fbuf)) $ drawMesh mb pl
-- draws children with transformed matrix
  mapM_ drawFrameA fbchildren
  where drawFrameA x = drawFrame x mloc tloc nmvM pl
        nmvM = ftransform (fframe fbuf) !*! mvM
        (Just mb) = fbuffer fbuf


drawSFrame :: Tree FrameBuffer -> [MF44] -> UniformLocation -> UniformLocation-> UniformLocation -> Pipeline -> DrawT IO ()

drawSFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Nothing})) fbchildren) _ _ _ _ pl = error "use drawFrame for non-skinned mesh"


drawSFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Just skel})) fbchildren) bsR tTex tOffset tBones pl = do
-- Offset matrices    
  forM_ (zip [0..] (map snd skel)) (\(a,b) -> setUniform b (tOffset+a) pl)
-- Transform matrices
  forM_ (zip [0..] bsRFrame) (\(a,b) -> setUniform b (tBones+a) pl)
  case ftexture fbuf of
    Just tex' -> do 
      setTextureBindings (IM.singleton defTex' tex')
      setUniform defTex' tTex pl
    Nothing -> return ()
  unless (isNothing (fbuffer fbuf)) $ drawMesh mb pl
  mapM_ drawFrameB fbchildren
  where drawFrameB x = drawSFrame x bsR tTex tOffset tBones pl
        (Just mb) = fbuffer fbuf
        bsRFrame = map (bsR !!) (map fst skel) 
