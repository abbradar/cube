module Engine.Loaders
       ( parseFromFile
       , runParser
       , loadMeshOBJ
       , loadFrameIX
       , loadFrameSX
       ) where


import qualified Data.Map as M
import Data.Foldable
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import GHC.Generics (Generic(..))
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Scientific as S
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..), parse)
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Foreign.Storable.Tuple ()
import Text.InterpolatedString.Perl6 (qq)

import Data.Wavefront
import Data.DirectX
import Data.DirectX.Data
import Data.DirectX.Core
import Data.Tree (Tree(..))

  
import Engine.Types
import Engine.Mesh

import Debug.Trace


 -- bones data for loading
data BonesData = BonesData { bname :: ByteString
                           , bweights :: [(Int, Float)]
                           , boffset :: MF44
                           }
               deriving (Generic, Show, Eq, Read)
 


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

-- loads animations

loadAnimation :: XTemplates -> FilePath -> FrameTree -> IO (Animation)
loadAnimation tmpls path skeleton = undefined--do
--  vals <- parseFromFile (directX' True tmpls) path
--  let anim = do
--        -- XXX: ignores all frames except the first one
--        as <- mapM (loadFrameTree' ldMesh) $ filter (\x -> dataTemplate x == "Animation") $ xData vals
--        let root = Frame { fmesh = Nothing
--                         , fname = Nothing
--                         , tname = Nothing
--                         , ftransform = identity
--                         }
--        return $ Node root fs
--  case frame of
--    Nothing -> fail "cannot find frames"
--    Just f -> return f


-- searches in the list of data an element with coinciding template
searchFieldT :: ByteString -> [Data] -> Maybe Data
searchFieldT nm = listToMaybe . filter (\dt -> dataTemplate dt == TName nm)

-- Main mesh loading function (from parsed data)
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
      
-- "frameMatrix" from data
getMatrix :: Maybe Data -> MF44
getMatrix d = getMatrix' d "frameMatrix"
-- matrix by name from data
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
-- loads list of (lists = triangles) of indices
loadInds :: Data -> Maybe [[Word32]]
loadInds dt = do
    VCustom (DA faces) <- M.lookup "faces" $ dataValues dt
    let inds1 x = fromMaybe [] $ do
          VDWord (DA ins) <- M.lookup "faceVertexIndices" x 
          return ins
        inds' = map inds1 faces
    return inds'

-- unpacks vertex
verts1 :: Values -> V3 Float
verts1 p = fromMaybe (V3 0 0 0) $ do
    VFloat (DV x1) <- M.lookup "x" p
    VFloat (DV y1) <- M.lookup "y" p
    VFloat (DV z1) <- M.lookup "z" p
    return (V3 x1 y1 z1)

-- loads list of vetices
loadVerts :: Data -> Maybe [V3 Float]
loadVerts dt = do
    VCustom (DA vertices) <- M.lookup "vertices" $ dataValues dt
    let verts' = map verts1 vertices
    return verts'

-- loads list of normals
loadNorms :: Data -> Maybe [V3 Float]
loadNorms dt = do 
    let dtn = searchFieldT "MeshNormals" $ dataChildren dt
    dtn1 <- maybeM "no normals data found" dtn
    VCustom (DA normals) <- M.lookup "normals" $ dataValues dtn1
    let norms' = map verts1 normals
    return norms'

-- loads list of texture coordinates
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

-- loads BonesData
loadWeights :: Data -> Maybe BonesData
loadWeights dt = do
    VString (DV bnm) <- M.lookup "transformNodeName" $ dataValues dt
    VDWord (DA binds) <- M.lookup "vertexIndices" $ dataValues dt
    VFloat (DA bweights) <- M.lookup "weights" $ dataValues dt
    --VCustom (DV boffset) <- M.lookup "matrixOffset" $ dataValues dt
    let boffset' = getMatrix' (Just dt) "matrixOffset"
    return $ BonesData bnm (zip (map fromIntegral binds) (bweights)) boffset'

-- loads list of bones
loadBones :: Data -> Maybe [BonesData]
loadBones dt = do
    let dtt = searchFieldT "XSkinMeshHeader" $ dataChildren dt
    dtt1 <- maybeM "no skin header found" dtt
    VWord (DV nbones) <- M.lookup "nMaxSkinWeightsPerVertex" $ dataValues dtt1
    when (nbones > 4) $ fail "more than 4 bones per vertex"
    mapM loadWeights $ filter (\dt' -> dataTemplate dt' == "SkinWeights") $ dataChildren dt

-- transform [BonesData] to the SkinData (as it will be used in the buffer)
bonesToVertices :: Int -> [BonesData] -> SkinData
bonesToVertices nverts bones =
  SkinData { sdvertices = lst
           , sdbones = map (\bdata -> (bname bdata, boffset bdata)) bones
           }
  where lst = map (\x -> (fmap fst x, fmap snd x)) lst1
        lst1 = VU.toList $ VU.create $ do
    -- output format
            res <- VUM.replicate nverts $ V4 (0, 0) (0, 0) (0, 0) (0, 0)
    -- number of bones for a given vertex
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
