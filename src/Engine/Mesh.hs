module Engine.Mesh
       ( Vert
       , Ind
       , Mesh
       , Frame(..)
       , FrameBuffer
       , vertices
       , indices
       , parseFromFile
       , runParser
       , Bone
       , Bones
       , generateSkeleton
       , loadMeshOBJ
       , loadFrameIX
       , loadFrameSX
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

import qualified Data.Map as M
import Data.Foldable
import qualified Data.IntMap.Strict as IM
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Control.Arrow
import Codec.Picture
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Foreign.Storable (Storable(..))
import GHC.Generics (Generic(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Scientific as S
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..), parse)
import Graphics.Caramia
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Foreign.Storable.Tuple ()
import Text.InterpolatedString.Perl6 (qq)

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
type Ind = I3
type VSkinData = (V4 Word8, V4 Float)

-- skin data for loading
data SkinData = SkinData { sdvertices :: [(V4 Word8, V4 Float)]
                         , sdbones :: [(ByteString, MF44)]
                         }
             deriving (Generic, Show, Eq, Read)


-- vertex with coords, normals and texture coords, the default one
data VertexD = VertexD { position :: !F3
                       , normal :: !F3
                       , texcoords :: !F2
                       }
             deriving (Generic, Show, Eq, Read)

-- skinned vertex
data SVertexD = SVertexD { sposition :: !F3
                         , snormal :: !F3
                         , stexcoords :: !F2
                         , bones :: !VSkinData
                         }
             deriving (Generic, Show, Eq, Read)

-- bones data for loading
data BonesData = BonesData { bname :: ByteString
                           , bweights :: [(Int, Float)]
                           , boffset :: MF44
                           }
               deriving (Generic, Show, Eq, Read)

-- bone for skeleton (name, dynamic matrix)
-- TODO: make an array for fast access and mutability
type Bone = (ByteString, MF44)
type Bones = [Bone]

-- mesh bone info: (id, static matrix)
-- TODO: make an array for fast access
type BonesInfo = [(Int, MF44)]


instance Storable VertexD where
  peek = gPeek
  poke = gPoke
  sizeOf = gSizeOf
  alignment = gAlignment

instance Storable SVertexD where
  peek = gPeek
  poke = gPoke
  sizeOf = gSizeOf
  alignment = gAlignment


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
------------------------------ LOADING ------------------------------------
---------------------------------------------------------------------------

parseFromFile :: Parser a -> FilePath -> IO a
parseFromFile parser path = do
  contents <- B.readFile path
  either (\e -> fail [qq|{path}: {e}|]) return $ runParser parser contents

runParser :: Parser a -> ByteString -> Either String a
runParser parser contents = tryParse True $ parse parser contents
  where tryParse _ (Fail left stack err) =
          let pos = B.length contents - B.length left
              part = B.take 10 left
          in Left [qq|Failed to parse at position {pos} ({part}), stack {stack}: {err}|]
        tryParse allowPartial (Partial f)
          | allowPartial = tryParse False $ f ""
          | otherwise = error "parseFromFile: impossible"
        tryParse _ (Done "" r) = Right r
        tryParse _ (Done left _) =
          let pos = B.length contents - B.length left
              part = B.take 10 left
          in Left [qq|Failed to parse at position {pos} ({part}): can't parse more|]

loadFrameIX :: XTemplates -> FilePath -> IO (Tree Frame)
loadFrameIX tmpls path = loadFrameX' tmpls path loadIMesh

loadFrameSX :: XTemplates -> FilePath -> IO (Tree Frame)
loadFrameSX tmpls path = loadFrameX' tmpls path $ loadSMesh

loadFrameX' :: XTemplates -> FilePath -> (Data -> Maybe Mesh) -> IO (Tree Frame)
loadFrameX' tmpls path ldMesh = do
  vals <- parseFromFile (directX' True tmpls) path
  let frame = do
        -- XXX: ignores all frames except the first one
        fs <- mapM (loadFrameTree' ldMesh) $ filter (\x -> dataTemplate x == "Frame") $ xData vals
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
searchFieldT nm = listToMaybe . filter (\dt -> dataTemplate dt == TName nm)

loadFrameTree' :: (Data -> Maybe Mesh) -> Data -> Maybe (Tree Frame)
loadFrameTree' ldMesh dt
  | dataTemplate dt == "Frame" = Just $ Node (Frame {..}) fchildren
  | otherwise = Nothing
  where
      fname = fromDName <$> dataName dt
      -- if fails to find one, sets to identity
      ftransform = getMatrix $ searchFieldT "FrameTransformMatrix" $ dataChildren dt
      fchildren = loadFrameTrees' ldMesh $ dataChildren dt
      meshdt = searchFieldT "Mesh" $ dataChildren dt
      fmesh = meshdt >>= ldMesh
      tname = do
        mdt <- meshdt
        ml <- searchFieldT "MeshMaterialList" $ dataChildren mdt
        loadMaterial ml
      

getMatrix :: Maybe Data -> MF44
getMatrix d = getMatrix' d "frameMatrix"

getMatrix' :: Maybe Data -> ByteString -> MF44
getMatrix' Nothing _ = identity
getMatrix' (Just d) mname = fromMaybe (error "invalid transform matrix") $ do
    VCustom (DV data1) <- M.lookup (MName mname) $ dataValues d
    VFloat (DA vals) <- M.lookup "matrix" data1
    let [a11,a12,a13,a14,a21,a22,a23,a24,a31,a32,a33,a34,a41,a42,a43,a44] = vals
    return  $ V4 (V4 a11 a12 a13 a14) (V4 a21 a22 a23 a24) (V4 a31 a32 a33 a34) (V4 a41 a42 a43 a44)

-- XXX: as for now only loads the texture filename from the ONE material
loadMaterial :: Data -> Maybe ByteString
loadMaterial dt = do
    ml <- searchFieldT "Material" $ dataChildren dt
    tname <- searchFieldT "TextureFilename" $ dataChildren ml
    VString (DV nm) <- M.lookup "filename" $ dataValues ( tname )
    return nm

maybeM :: Monad m => String -> Maybe a -> m a
maybeM err Nothing = fail err
maybeM _ (Just a) = return a


-- functions for mesh loading
loadInds :: Data -> Maybe [[Word32]]
loadInds dt = do
    VCustom (DA faces) <- M.lookup "faces" $ dataValues dt
    let inds1 x = fromMaybe [] $ do
          VDWord (DA ins) <- M.lookup "faceVertexIndices" x 
          return ins
        inds' = map inds1 faces
    return inds'

verts1 :: Values -> V3 Float
verts1 p = fromMaybe (V3 0 0 0) $ do
    VFloat (DV x1) <- M.lookup "x" p
    VFloat (DV y1) <- M.lookup "y" p
    VFloat (DV z1) <- M.lookup "z" p
    return (V3 x1 y1 z1)

loadVerts :: Data -> Maybe [V3 Float]
loadVerts dt = do
    VCustom (DA vertices) <- M.lookup "vertices" $ dataValues dt
    let verts' = map verts1 vertices
    return verts'

loadNorms :: Data -> Maybe [V3 Float]
loadNorms dt = do 
    let dtn = searchFieldT "MeshNormals" $ dataChildren dt
    dtn1 <- maybeM "no normals data found" dtn
    VCustom (DA normals) <- M.lookup "normals" $ dataValues dtn1
    let norms' = map verts1 normals
    return norms'

loadCoords :: Data -> Maybe [V2 Float]
loadCoords dt = do
    let dtt = searchFieldT "MeshTextureCoords" $ dataChildren dt
    dtt1 <- maybeM "no texcoords data found" dtt
    VCustom (DA coordinates) <- M.lookup "textureCoords" $ dataValues dtt1
    let coords1 x = fromMaybe (V2 0 0) $ do
          VFloat (DV x1) <- M.lookup "u" x 
          VFloat (DV y1) <- M.lookup "v" x 
          return (V2 x1 y1)
        coords' = map coords1 coordinates
    return coords'

loadWeights :: Data -> Maybe BonesData
loadWeights dt = do
    VString (DV bnm) <- M.lookup "transformNodeName" $ dataValues dt
    VDWord (DA binds) <- M.lookup "vertexIndices" $ dataValues dt
    VFloat (DA bweights) <- M.lookup "weights" $ dataValues dt
    --VCustom (DV boffset) <- M.lookup "matrixOffset" $ dataValues dt
    let boffset' = getMatrix' (Just dt) "matrixOffset"
    return $ BonesData bnm (zip (map fromIntegral binds) (bweights)) boffset'

loadBones :: Data -> Maybe [BonesData]
loadBones dt = do
    let dtt = searchFieldT "XSkinMeshHeader" $ dataChildren dt
    dtt1 <- maybeM "no skin header found" dtt
    VWord (DV nbones) <- M.lookup "nMaxSkinWeightsPerVertex" $ dataValues dtt1
    when (nbones > 4) $ fail "more than 4 bones per vertex"
    mapM loadWeights $ filter (\dt' -> dataTemplate dt' == "SkinWeights") $ dataChildren dt

bonesToVertices :: Int -> [BonesData] -> SkinData


bonesToVertices nverts bones =
  SkinData { sdvertices = lst
           , sdbones = map (\bdata -> (bname bdata, boffset bdata)) bones
           }
  where lst = map (\x -> (fmap fst x, fmap snd x)) lst1
        lst1 = VU.toList $ VU.create $ do
    --      res <- VUM.replicate nverts $ V4 (0, 0) (0, 0) (0, 0) (0, 0)
            res <- VUM.replicate nverts $ V4 (0, 0) (0, 0) (0, 0) (0, 0)
    --        res <- VUM.replicate nverts $ (V4 0 0 0 0, V4 0 0 0 0)
            indices <- VUM.replicate nverts (0 :: Int)
            forM_ (zip [0..] bones) $ \(boneIdx, bone) -> do
              forM_ (bweights bone) $ \(vertIdx, f) -> do
              -- FIXME: check that vertIdx is okay and fail gracefully
                lastIdx <- VUM.read indices vertIdx
                VUM.write indices vertIdx (lastIdx + 1)
                let n = (boneIdx, f)
                    modifyBones (V4 a b c d) = case lastIdx of
                      0 -> V4 n b c d
                      1 -> V4 a n c d
                      2 -> V4 a b n d
                      3 -> V4 a b c n
                      _ -> error "modifyBones: impossible"
                VUM.modify res modifyBones vertIdx
            return res
 

-- load Mesh with 3-indexed faces and vertex normals
-- all incorrect vertices become (0 0 0)
loadIMesh :: Data -> Maybe Mesh
loadIMesh dt = do
    --faces
    inds <- loadInds dt
    --vertices
    verts <- loadVerts dt
    --normals
    norms <- loadNorms dt
    --coords
    coords <- loadCoords dt
    -- gluing together
    indlist <- forM inds $ \case
      [a, b, c] -> return (fromIntegral <$> V3 a b c)
      _ -> fail "invalid points number"
    let vertlist = zipWith3 VertexD verts norms coords

    return $ IM IMesh { vertices = VS.fromList vertlist
                , indices = VS.fromList indlist
                }

-- load skinned Mesh with 3-indexed faces and vertex normals
-- all incorrect vertices become (0 0 0)
loadSMesh :: Data -> Maybe Mesh
loadSMesh dt = do
    --faces
    inds <- loadInds dt
    --vertices
    verts <- loadVerts dt
    --normals
    norms <- loadNorms dt
    --coords
    coords <- loadCoords dt
    -- gluing together
    skdata <- bonesToVertices (length verts) <$> loadBones dt

    indlist <- forM inds $ \case
      [a, b, c] -> return (fromIntegral <$> V3 a b c)
      _ -> fail "invalid points number"
    let vertlist = zipWith4 SVertexD verts norms coords (sdvertices skdata)

    return $ SM SMesh { svertices = VS.fromList vertlist
                , sindices = VS.fromList indlist
                , sbones = sdbones skdata
                }



loadFrameTrees' :: (Data -> Maybe Mesh) -> [Data] -> [Tree Frame]
loadFrameTrees' loadMesh dt = mapMaybe (loadFrameTree' loadMesh) dt

loadMeshOBJ :: FilePath -> IO IMesh
loadMeshOBJ path = do
  vals <- parseFromFile wavefrontOBJ path
  let WFModel {..} = extractModel vals
      indlist = map (fmap fromIntegral) wfIndices
      --TODO: check different length
      convertF = map (fmap S.toRealFloat)
      vertlist = zipWith3 VertexD (convertF wfVertices) (convertF wfNormals) (repeat (V2 0.0 0.0))
      --vertlist = zip wfVertices wfNormals
  return IMesh { vertices = VS.fromList vertlist
               , indices = VS.fromList indlist
               }

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

-- initializes frame buffer. fdir is for the texture.
-- Maybe we should put it somwhere else

initIFrameBuffer :: FilePath -> Frame -> IO FrameBuffer
initIFrameBuffer fdir frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  let fname = fmap B.unpack (tname frame)
      name' = fmap (fdir ++ ) fname
  tex <- maybe (return Nothing) loadTex name'
  return $ FrameBuffer { fframe = frame, fbuffer = mbuf, fmeshbonesinfo = Nothing, ftexture = tex }

initSFrameBuffer :: FilePath -> [ByteString] -> Frame -> IO FrameBuffer
initSFrameBuffer fdir bs frame = do
  mbuf <- mapM initMeshBuffer (fmesh frame)
  let fname = fmap B.unpack (tname frame)
      name' = fmap (fdir ++ ) fname
      binfo = genBones bs (fmesh frame)
  tex <- maybe (return Nothing) loadTex name'
  return $ FrameBuffer { fframe = frame, fbuffer = mbuf, fmeshbonesinfo = Just binfo, ftexture = tex }
  where 
    genBones :: [ByteString] -> Maybe Mesh -> BonesInfo 
    genBones _ Nothing = []
    genBones _ (Just (IM _)) = []
 -- FROMJUST OLOLO FIXMI
    genBones bones (Just (SM smesh)) = map (first $ \name -> fromJust (findIndex (== name) bones)) $ sbones smesh
      

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


drawSFrame :: Tree FrameBuffer -> [MF44] -> UniformLocation -> UniformLocation -> UniformLocation-> Pipeline -> DrawT IO ()

drawSFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Nothing})) fbchildren) _ tloc _ _ pl = error "use drawFrame for non-skinned mesh"


drawSFrame (Node (fbuf@(FrameBuffer {fmeshbonesinfo = Just skel})) fbchildren) bsR tloc tOff tTrans pl = do
-- Offset matrices    
  forM_ (zip [0..3] (map snd bsInfo)) (\(a,b) -> setUniform b (tOff+a) pl)
-- Transform matrices
  forM_ (zip [0..] bsRFrame) (\(a,b) -> setUniform b (tTrans+a) pl)

  case ftexture fbuf of
    Just tex' -> do 
      setTextureBindings (IM.singleton defTex' tex')
      setUniform defTex' tloc pl
    Nothing -> return ()
  unless (isNothing (fbuffer fbuf)) $ drawMesh mb pl
  mapM_ drawFrameB fbchildren
  where drawFrameB x = drawSFrame x bsR tloc tOff tTrans pl
        (Just mb) = fbuffer fbuf
        bsInfo = (fromJust $ fmeshbonesinfo fbuf)
        bsRFrame = map (bsR !!) (map fst bsInfo) 


