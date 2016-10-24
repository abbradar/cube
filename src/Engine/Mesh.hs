module Engine.Mesh
       ( Vert
       , Ind
       , Mesh
       , Frame
       , FrameBuffers
       , vertices
       , indices
       , loadMeshOBJ
       , loadFrameX
       , MeshBuffer
       , initMeshBuffer
       , drawMesh
       , initFrame
       , drawFrame
       ) where

import Control.Monad
import Foreign.Storable (Storable(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Scientific as S
import Text.Trifecta.Parser (parseFromFile)
import Graphics.Caramia
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix

import Data.ByteString (ByteString)
import qualified Data.Map as M
import Data.Maybe
import Data.Wavefront
import Data.DirectX
import Data.DirectX.Data
import Data.DirectX.Core
import Data.Tree
import Engine.Types

import Debug.Trace

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * VS.length vec

type Vert = V2 F3
type Ind = I3

data Mesh = Mesh { vertices :: Vector Vert
                 , indices :: Vector Ind
                 } deriving (Show, Eq, Read)

data Frame = Frame { fmesh :: Maybe Mesh
                   , fname :: Maybe ByteString
                   , ftransform :: MF44
                   , fchildren :: [Frame]
                   } deriving (Show, Eq, Read)

data FrameBuffers = FrameBuffers { fframe :: Frame
                  , fbuffer :: Maybe MeshBuffer
                  , fbchildren :: [FrameBuffers]
                  }

loadFrameX :: XTemplates -> FilePath -> IO Frame
loadFrameX tmpls name = do
  values <- parseFromFile (directX' tmpls) name
  case values of
    Nothing -> fail $ "loadMesh: failed to load " ++ name
    Just r -> do
    -- WARNING: ignores all frames except the first one
      let frame = loadFrame $ head $ xData $ r
        in case frame of
          Nothing -> fail "cannot find frames"
          Just f -> return f

-- searches in the list of data an element with coinciding template
searchFieldT :: ByteString -> [Data] -> Maybe Data
searchFieldT _ [] = Nothing
searchFieldT nm (dt:dts)
  | dataTemplate dt /= (TName nm) = searchFieldT nm dts
  | otherwise = Just dt

loadFrame :: Data -> Maybe Frame
loadFrame dt
  | dataTemplate dt == "Frame" = Just Frame{..}
  | otherwise = Nothing
  where
      fname = fromDName <$> dataName dt
    -- if fails to find one, sets to identity
      ftransform = getMatrix $ searchFieldT "FrameTransformMatrix" (dataChildren dt)
      fchildren = loadFrames (dataChildren dt)
      fmesh = loadMesh $ searchFieldT "Mesh" (dataChildren dt)
      fbuffer = Nothing

getMatrix :: Maybe Data -> MF44
getMatrix Nothing = identity
getMatrix (Just d) = fromMaybe (error "invalid transform matrix") $ do
    VCustom (DV data1) <- M.lookup "frameMatrix" (dataValues d)
    VFloat (DA vals) <- M.lookup "matrix" data1
    let a11:a12:a13:a14:a21:a22:a23:a24:a31:a32:a33:a34:a41:a42:a43:a44:[] = vals
    return  $ V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)

unMonad :: Monad m => String -> Maybe a -> m a
unMonad err Nothing = fail err
unMonad _ (Just a) = return a

-- load Mesh with 3-indexed faces and vertex normals
-- all incorrect vertexes become (0 0 0)
loadMesh :: Maybe Data -> Maybe Mesh
loadMesh Nothing = Nothing
loadMesh (Just dt) = do
    --faces
    VCustom (DA faces) <- M.lookup "faces" (dataValues dt)
    let inds1 x = fromMaybe [] $ do
          VDWord (DA ins) <- M.lookup "faceVertexIndices" x 
          return ins
        inds = map inds1 faces
    --vertices
    VCustom (DA vertices) <- M.lookup "vertices" (dataValues dt)
    let verts1 x = fromMaybe (V3 0 0 0) $ do
          VFloat (DV x1) <- M.lookup "x" x 
          VFloat (DV y1) <- M.lookup "y" x 
          VFloat (DV z1) <- M.lookup "z" x 
          return (V3  x1 y1 z1)
        verts = map verts1 vertices
    --normals
    let dtn = searchFieldT "MeshNormals" (dataChildren dt)
    dtn1 <- unMonad "no normals data found" dtn
    VCustom (DA normals) <- M.lookup "normals" (dataValues dtn1)
    let norms1 x = fromMaybe (V3 0 0 0) $ do
          VFloat (DV x1) <- M.lookup "x" x 
          VFloat (DV y1) <- M.lookup "y" x 
          VFloat (DV z1) <- M.lookup "z" x 
          return (V3 x1 y1 z1)
        norms = map norms1 normals
    -- gluing together
    let indlist = map (\[a, b, c]->(V3 (fromIntegral a) (fromIntegral b) (fromIntegral c))) inds
        vertlist = zipWith V2 verts norms
    return Mesh { vertices = VS.fromList vertlist, indices = VS.fromList indlist }

loadFrames :: [Data] -> [Frame]
loadFrames dtl = mapMaybe loadFrame dtl

loadMeshOBJ :: FilePath -> IO Mesh
loadMeshOBJ name = do
  values <- parseFromFile wavefrontOBJ name
  case values of
    Nothing -> fail "loadMeshOBJ: failed to load mesh"
    Just r -> do
      let WFModel{..} = extractModel r
          indlist = map (fmap fromIntegral) wfIndices
          --TODO: check different length
          convertF = map (fmap S.toRealFloat)
          vertlist = zipWith V2 (convertF wfVertices) (convertF wfNormals)
          --vertlist = zip wfVertices wfNormals
      return Mesh { vertices = VS.fromList vertlist
                  , indices = VS.fromList indlist
                  }

data MeshBuffer = MeshBuffer { mvao :: VAO
                             , mbuffer :: Buffer
                             , indOffset :: Int
                             , indNumber :: Int
                             } 
instance Show MeshBuffer where
  show buf = "mesh buffer"

initMeshBuffer :: Mesh -> IO MeshBuffer
initMeshBuffer mesh = do
  vao <- newVAO
  buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                          , size = sizeOfV (vertices mesh) + sizeOfV(indices mesh)
                                          }
  uploadVector (vertices mesh) 0 buff
  sourceVertexData buff defaultSourcing { components = 3
                                        , stride = sizeOf (undefined :: Vert)
                                        , attributeIndex = 0
                                        , sourceType = SFloat
                                        } vao
  
  sourceVertexData buff defaultSourcing { offset = sizeOf (undefined :: (V3 Float))
                                        , components = 3
                                        , stride = sizeOf (undefined :: Vert)
                                        , attributeIndex = 1
                                        , sourceType = SFloat
                                        } vao

  uploadVector (indices mesh) (sizeOfV (vertices mesh)) buff
  return MeshBuffer { mvao = vao
                    , mbuffer = buff
                    , indOffset = sizeOfV (vertices mesh)
                    , indNumber = 3*(VS.length (indices mesh))
                    }

drawMesh :: MeshBuffer -> Pipeline -> DrawT IO ()
drawMesh mesh pl =
  drawR drawCommand { primitiveType = Triangles
                    , primitivesVAO = mvao mesh
                    , numIndices = indNumber mesh
                    -- , sourceData = Primitives 0
                    , sourceData = PrimitivesWithIndices (mbuffer mesh) (indOffset mesh) IWord16
                    }

initFrame :: Frame -> IO FrameBuffers
initFrame frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  chldrn <- mapM initFrame (fchildren frame)
  return FrameBuffers{ fframe = frame, fbuffer = mbuf, fbchildren = chldrn }

drawFrame :: FrameBuffers -> UniformLocation -> MF44 -> Pipeline -> DrawT IO ()
drawFrame fbuf loc mvM pl = do
  setUniform nmvM loc pl
  unless (isNothing (fbuffer fbuf)) $ drawMesh mb pl
  mapM_ drawFrameA (fbchildren fbuf)
  where drawFrameA x = drawFrame x loc nmvM pl
        nmvM = (ftransform (fframe fbuf)) !*! mvM
        (Just mb) = fbuffer fbuf
