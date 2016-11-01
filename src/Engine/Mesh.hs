module Engine.Mesh
       ( Vert
       , Ind
       , Mesh
       , Frame
       , FrameBuffer
       , vertices
       , indices
       , loadMeshOBJ
       , loadFrameX
       , MeshBuffer
       , initMeshBuffer
       , drawMesh
       , initFrameBuffer
       , drawFrame
       , FrameTree
       , FrameBufferTree
       ) where

import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Control.Monad
import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Scientific as S
import Text.Trifecta.Parser (parseFromFile)
import Graphics.Caramia
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix

import Foreign.Storable.Generic
import Data.Wavefront
import Data.DirectX
import Data.DirectX.Data
import Data.DirectX.Core
import Data.Tree (Tree(..))
import Engine.Types

import Debug.Trace

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * VS.length vec

type Vert = V2 F3

-- vertex with coords, normals and texture coords, the default one
data Vertexd = Vertexd { position :: !F3 
                       , normal :: !F3
                       , texcoords :: !F2
                       }
             deriving (Generic, Show, Eq, Read)

instance Storable Vertexd where
  peek = gPeek
  poke = gPoke
  sizeOf = gSizeOf
  alignment = gAlignment

type Ind = I3

data Mesh = Mesh { vertices :: Vector Vertexd
                 , indices :: Vector Ind
                 }
          deriving (Show, Eq, Read)

data Frame = Frame { fmesh :: Maybe Mesh
                   , fname :: Maybe ByteString
                   , tname :: Maybe ByteString
                   , ftransform :: MF44
                   }
           deriving (Show, Eq, Read)

type FrameTree = Tree Frame
type FrameBufferTree = Tree FrameBuffer

data FrameBuffer = FrameBuffer { fframe :: Frame
                               , fbuffer :: Maybe MeshBuffer
                               , ftexture :: Maybe Texture
                               }

loadFrameX :: XTemplates -> FilePath -> IO (Tree Frame)
loadFrameX tmpls path = do
  vals <- parseFromFile (directX' tmpls) path
  case vals of
    Nothing -> fail $ "loadMesh: failed to load " ++ path
    Just r -> do
      let frame = do
            -- XXX: ignores all frames except the first one
            fs <- mapM loadFrameTree $ filter (\x -> dataTemplate x == "Frame") $ xData r
            let root = Frame { fmesh = Nothing
                             , fname = Nothing
                             , tname = Nothing
                             , ftransform = identity
                             }
            return $ Node root fs
      case frame of
        Nothing -> fail "cannot find frames"
        Just f -> return f

-- searches in the list of data an element with coinciding template
searchFieldT :: ByteString -> [Data] -> Maybe Data
searchFieldT _ [] = Nothing
searchFieldT nm (dt:dts)
  | dataTemplate dt /= TName nm = searchFieldT nm dts
  | otherwise = Just dt

loadFrameTree :: Data -> Maybe (Tree Frame)
loadFrameTree dt
  | dataTemplate dt == "Frame" = Just $ Node (Frame {..}) fchildren
  | otherwise = Nothing
  where
      fname = fromDName <$> dataName dt
      -- if fails to find one, sets to identity
      ftransform = getMatrix $ searchFieldT "FrameTransformMatrix" (dataChildren dt)
      fchildren = loadFrameTrees $ dataChildren dt
      meshdt = searchFieldT "Mesh" (dataChildren dt)
      fmesh = meshdt >>= loadMesh
      tname = do
        mdt <- meshdt
        ml <- searchFieldT "MeshMaterialList" (dataChildren mdt)
        loadMaterial ml
      

getMatrix :: Maybe Data -> MF44
getMatrix Nothing = identity
getMatrix (Just d) = fromMaybe (error "invalid transform matrix") $ do
    VCustom (DV data1) <- M.lookup "frameMatrix" (dataValues d)
    VFloat (DA vals) <- M.lookup "matrix" data1
    let [a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34,a41,a42,a43,a44] = vals
    return  $ V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)

-- XXX: as for now only loads the texture filename from the ONE material
loadMaterial :: Data -> Maybe ByteString
loadMaterial dt = do
    ml <- (searchFieldT "Material" (dataChildren dt))
    tname <- searchFieldT "TextureFilename" (dataChildren ml)
    VString (DV nm) <- M.lookup "filename" $ dataValues ( tname )
    return nm

maybeM :: Monad m => String -> Maybe a -> m a
maybeM err Nothing = fail err
maybeM _ (Just a) = return a

-- load Mesh with 3-indexed faces and vertex normals
-- all incorrect vertexes become (0 0 0)
loadMesh :: Data -> Maybe Mesh
loadMesh dt = do
    --faces
    VCustom (DA faces) <- M.lookup "faces" (dataValues dt)
    let inds1 x = fromMaybe [] $ do
          VDWord (DA ins) <- M.lookup "faceVertexIndices" x 
          return ins
        inds = map inds1 faces
    --vertices
    VCustom (DA vertices) <- M.lookup "vertices" (dataValues dt)
    let verts1 p = fromMaybe (V3 0 0 0) $ do
          VFloat (DV x1) <- M.lookup "x" p
          VFloat (DV y1) <- M.lookup "y" p
          VFloat (DV z1) <- M.lookup "z" p
          return (V3 x1 y1 z1)
        verts = map verts1 vertices
    --normals
    let dtn = searchFieldT "MeshNormals" (dataChildren dt)
    dtn1 <- maybeM "no normals data found" dtn
    VCustom (DA normals) <- M.lookup "normals" (dataValues dtn1)
    let norms = map verts1 normals
    --coords
    let dtt = searchFieldT "MeshTextureCoords" (dataChildren dt)
    dtt1 <- maybeM "no texcoords data found" dtt
    VCustom (DA coordinates) <- M.lookup "textureCoords" (dataValues dtt1)
    let coords1 x = fromMaybe (V2 0 0) $ do
          VFloat (DV x1) <- M.lookup "u" x 
          VFloat (DV y1) <- M.lookup "v" x 
          return (V2 x1 y1)
        coords = map coords1 coordinates
    -- gluing together
    indlist <- forM inds $ \case
      [a, b, c] -> return (fromIntegral <$> V3 a b c)
      _ -> fail "invalid points number"
    let vertlist = zipWith3 Vertexd verts norms coords

    return Mesh { vertices = VS.fromList vertlist
                , indices = VS.fromList indlist
                }

loadFrameTrees :: [Data] -> [Tree Frame]
loadFrameTrees dtl = mapMaybe loadFrameTree dtl

loadMeshOBJ :: FilePath -> IO Mesh
loadMeshOBJ path = do
  vals <- parseFromFile wavefrontOBJ path
  case vals of
    Nothing -> fail "loadMeshOBJ: failed to load mesh"
    Just r -> do
      let WFModel {..} = extractModel r
          indlist = map (fmap fromIntegral) wfIndices
          --TODO: check different length
          convertF = map (fmap S.toRealFloat)
          vertlist = zipWith3 Vertexd (convertF wfVertices) (convertF wfNormals) (repeat (V2 0.0 0.0))
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
  show buf = "MeshBuffer"

initMeshBuffer :: Mesh -> IO MeshBuffer
initMeshBuffer mesh = do
  vao <- newVAO
  buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                          , size = sizeOfV (vertices mesh) + sizeOfV (indices mesh)
                                          }
  uploadVector (vertices mesh) 0 buff
  sourceVertexData buff defaultSourcing { components = 3
                                        , stride = sizeOf (undefined :: Vertexd)
                                        , attributeIndex = 0
                                        , sourceType = SFloat
                                        } vao
  
  sourceVertexData buff defaultSourcing { offset = sizeOf (undefined :: (V3 Float))
                                        , components = 3
                                        , stride = sizeOf (undefined :: Vertexd)
                                        , attributeIndex = 1
                                        , sourceType = SFloat
                                        } vao
  sourceVertexData buff defaultSourcing { offset = 2 * sizeOf (undefined :: (V3 Float))
                                        , components = 2
                                        , stride = sizeOf (undefined :: Vertexd)
                                        , attributeIndex = 2
                                        , sourceType = SFloat
                                        } vao

  uploadVector (indices mesh) (sizeOfV (vertices mesh)) buff
  return MeshBuffer { mvao = vao
                    , mbuffer = buff
                    , indOffset = sizeOfV (vertices mesh)
                    , indNumber = 3 * VS.length (indices mesh)
                    }

drawMesh :: MeshBuffer -> Pipeline -> DrawT IO ()
drawMesh mesh pl =
  drawR drawCommand { primitiveType = Triangles
                    , primitivesVAO = mvao mesh
                    , numIndices = indNumber mesh
                    , sourceData = PrimitivesWithIndices (mbuffer mesh) (indOffset mesh) IWord16
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

initFrameBuffer :: FilePath -> Frame -> IO FrameBuffer
initFrameBuffer fdir frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  let fname = fmap B.unpack (tname frame)
      name' = fmap (fdir ++ ) fname
  tex <- maybe (return Nothing) loadTex name'
  return FrameBuffer { fframe = frame, fbuffer = mbuf, ftexture = tex }


defTex' :: Int
defTex' = 0

drawFrame :: Tree FrameBuffer -> UniformLocation -> UniformLocation -> MF44 -> Pipeline -> DrawT IO ()
drawFrame (Node fbuf fbchildren) mloc tloc mvM pl = do
  setUniform nmvM mloc pl
  case ftexture fbuf of
    Just tex' -> do 
      setTextureBindings (IM.singleton defTex' tex')
      setUniform defTex' tloc pl
    Nothing -> return ()
  unless (isNothing (fbuffer fbuf)) $ drawMesh mb pl
  mapM_ drawFrameA fbchildren
  where drawFrameA x = drawFrame x mloc tloc nmvM pl
        nmvM = ftransform (fframe fbuf) !*! mvM
        (Just mb) = fbuffer fbuf
