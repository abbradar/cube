module Engine.Chunk
  ( Chunk(..)
  , Block
  , generateChunk
  , getClosestGround
  , isEmpty
  , chunkWidth
  , chunkHeight
  , getBlock
  , getBlock'
  , sGenerateMeshFromBlocks
  , generateMeshFromBlocks
  ) where


import Data.Word
import Linear
--import GHC.Generics (Generic(..))
import Foreign.Storable.Tuple ()
import Numeric.Noise.Perlin


import qualified Data.Map as M
import Data.Foldable
import Data.Maybe
import Data.Word
import Data.List
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import GHC.Generics (Generic(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Scientific as S
import Data.Attoparsec.ByteString.Char8 (Parser, IResult(..), parse)
import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Matrix
import Foreign.Storable.Tuple ()
--import Text.InterpolatedString.Perl6 (qq)

--import Data.Wavefront
--import Data.DirectX
--import Data.DirectX.Data
--import Data.DirectX.Core
--import Data.Tree (Tree(..))

  
import Engine.Types
import Engine.Mesh
import Engine.Biome

import Debug.Trace

-- now just a chunk

type Block = Word8

--type IArray2 = V.Vector ( V.Vector Block )
--type IArray3 = V.Vector IArray2
type ChunkBlocks = V.Vector Block
type PosBlock = WorldPos -> Block


data Chunk = Chunk { blocks :: ChunkBlocks
                   , pos :: HorizontalPos
                   } deriving (Show, Eq, Read)


-- constants
-- TODO: make some of them variables
chunkWidth :: Int
chunkHeight :: Int

chunkWidth = 16
chunkHeight = 32

  
---------------------------------------------------------------  
------------------------ GAME LOGIC----------------------------  
---------------------------------------------------------------  
  
getBlock :: ChunkBlocks -> WorldPos -> Block
getBlock arr (V3 a b c) = arr V.! (c + chunkHeight*(b + chunkWidth*a))

  
getBlock' :: Chunk -> WorldPos -> Block
getBlock' ch (V3 a b c) = (blocks ch) V.! (c + chunkHeight*(b + chunkWidth*a))
  
  
isEmpty :: Chunk -> WorldPos -> Bool
isEmpty (Chunk bl _) pos = getBlock bl pos == 0 

  
isEmpty' :: ChunkBlocks -> WorldPos -> Bool
isEmpty' bl pos = getBlock bl pos == 0 

-- if the input block is solid returns itself
getClosestGround :: Chunk -> WorldPos -> WorldPos
getClosestGround lsc@(Chunk bl _) (V3 a b c) = (V3 a b (findBottom c))
  where
    findBottom 1 = 1
    findBottom z = if(isEmpty lsc (V3 a b z)) then findBottom (z-1) else z

getClosestGround'' :: PosBlock -> WorldPos -> WorldPos
getClosestGround'' blocks (V3 a b c) = (V3 a b (findBottom c))
  where
    findBottom 1 = 1
    findBottom z = if(blocks (V3 a b z) == 0) then findBottom (z-1) else z


getClosestGround' :: ChunkBlocks -> WorldPos -> WorldPos
getClosestGround' bl (V3 a b c) = (V3 a b (findBottom c))
  where
    findBottom 1 = 1
    findBottom z = if(isEmpty' bl (V3 a b z)) then findBottom (z-1) else z


---------------------------------------------------------------  
------------------------ SYSTEM -------------------------------  
---------------------------------------------------------------  

-- landscape generation

-- supplementary type
-- edges and faces array for surface nets
-- a = 0 1 2 3 4 5. face = (mod a 3)-1. edge = quot a 3. 1 = empty
type IFaceArray3 = V.Vector ( V.Vector ( V.Vector (V.Vector Int)))
type VisibleFaces = (IFaceArray3, [Int])

faceFromVF n (ef, ns) = (ef V.! x V.! y V.! z) !! nv
  where
    x = quot (quot (quot n' 3) chunkHeight) chunkWidth
    y = mod (quot (quot n' 3) chunkHeight) chunkWidth
    z = mod (quot n' 3) chunkHeight
    nv = mod n' 3
    n' = ns !! n



  
getElem3 :: V.Vector( V.Vector (V.Vector a)) -> Int3 -> a
getElem3 arr (V3 a b c) = arr V.! a  V.! b  V.! c
  
  
-- creates a face from four vertices with orientation v0 v1 v2 v3
generateFace :: (V4 F3, V4 F3) -> Int -> ([Ind], [VertexD])
generateFace ((V4 v1 v2 v3 v4), (V4 n1 n2 n3 n4)) io' = ([(V3 (io+0) (io+1) (io+2)), (V3 (io+0) (io+2) (io+3))], [(VertexD v1 n1 (V2 0.0 1.0)), (VertexD v2 n2 (V2 0.0 0.0)),
                                                                                                                  (VertexD v3 n3 (V2 1.0 0.0)), (VertexD v4 n4 (V2 1.0 1.0))])
  where io = fromIntegral io'


---------------------------------minecraft style generation with bad normals---------------------------------------
  
-- creates all faces of the block which are visible
generateFacesFromBlock :: ChunkBlocks -> Int3 -> (([Ind], [VertexD]), Int) -> (([Ind], [VertexD]), Int)
generateFacesFromBlock blocks (V3 x y z) (list, indOffset) = addFace indOffset 5 list
  where
    addFace :: Int -> Int -> ([Ind], [VertexD]) -> (([Ind], [VertexD]), Int)
    addFace io (-1) list' = (list', io)
    addFace io n list' = if(getBlock blocks (fNorms n + (V3 x y z)) == 0) then (join' (newFace n io) (addFace (io+4) (n-1) list')) else addFace io (n-1) list'

    join' :: ([Ind], [VertexD]) -> (([Ind], [VertexD]), Int) -> (([Ind], [VertexD]), Int)
    join' (a',b') ((c',d'),n) = ((c'++a', d'++b'),n)

    fNorms n = ([V3 0 0 1, V3 (-1) 0 0, V3 0 1 0, V3 1 0 0, V3 0 (-1) 0, V3 0 0 (-1)]) !! n
  
    newFace n io
      | n < 6 && n >= 0 = generateFace (vertNorm (faces' !! n)) io
      | otherwise = ([], [])

    faces' = [(0,1,2,3), (0,4,5,1), (3,7,4,0), (2,6,7,3), (1,5,6,2), (5,4,7,6)]

    vertNorm :: (Int, Int, Int, Int) -> (V4 F3, V4 F3)
    vertNorm (n0, n1, n2, n3) = ((V4 (vert!!n0) (vert!!n1) (vert!!n2) (vert!!n3)), (V4 (norms!!n0) (norms!!n1) (norms!!n2) (norms!!n3)))
    
    vert :: [F3]
    vert = map (+ (V3 x' y' z')) [V3 0 (1.0) 0, V3 0 0 0, V3 (1.0) 0 0, V3 (1.0) (1.0) 0, V3 0 (1.0) (-1.0), V3 0 0 (-1.0), V3 (1.0) 0 (-1.0), V3 (1.0) (1.0) (-1.0)]
    norms :: [F3]
    norms = [V3 (-c) c c, V3 (-c) (-c) c, V3 c c c, V3 c c c, V3 (-c) c (-c), V3 (-c) (-c) (-c), V3 c (-c) (-c), V3 c c (-c)]
    c = 0.57735
    x' = fromIntegral (x - quot chunkWidth 2)
    y' = fromIntegral (y - quot chunkWidth 2)
    z' = fromIntegral (z - quot chunkHeight 2)

  
-- creates faces from all blocks from a colomn
generateFacesFromColomn :: ChunkBlocks -> Int3 -> (([Ind], [VertexD]), Int) -> (([Ind], [VertexD]), Int)
generateFacesFromColomn blocks pos@(V3 x y z) (list, io) = if(getBlock blocks (V3 x y (z-1)) > 0 &&
                                                              (getBlock blocks (V3 (x+1) y (z-1)) > 0 || getBlock blocks (V3 (x-1) y (z-1)) > 0 ||
                                                               getBlock blocks (V3 x (y+1) (z-1)) > 0 || getBlock blocks (V3 x (y-1) (z-1)) > 0))
  then (generateFacesFromBlock blocks (V3 x y z) generateBelow) else (generateFacesFromBlock blocks (V3 x y z) (list, io))
  where
    generateBelow = (generateFacesFromBlock blocks (V3 x y (z-1)) (list, io))

-- creates a mesh for a chunk by colomns, "minecraft" method with bad normals
-- TODO: add boundary colomns
generateMeshFromBlocks :: ChunkBlocks -> Mesh
generateMeshFromBlocks blocks = IM (IMesh (VS.fromList $ snd vtxData) (VS.fromList $ fst vtxData))
  where
    vtxData = fst $ genColomnsRec cw1 (([], []), 0)
    genColomnsRec :: Int -> (([Ind], [VertexD]), Int) -> (([Ind], [VertexD]), Int)
    genColomnsRec m (list, io)
      | m == cw1*(cw1+1)-1 = generateFacesFromColomn blocks (getClosestGround' blocks (V3 (quot m cw1) (mod m cw1 + 1) (chunkHeight-1))) (list, io)
      | otherwise = generateFacesFromColomn blocks (getClosestGround' blocks (V3 (quot m cw1) (mod m cw1 + 1) (chunkHeight-1))) (genColomnsRec (m+1) (list, io))
    cw1 = chunkWidth-2

---------------------------------naive surface nets---------------------------------------

unpackIndex :: Int -> (Int3, Int)
unpackIndex n' = (V3 x y z, n)
  where
    x = quot (quot (quot n' 3) chunkHeight) chunkWidth
    y = mod (quot (quot n' 3) chunkHeight) chunkWidth
    z = mod (quot n' 3) chunkHeight
    n = mod n' 3


-- generates mesh from a face, uses knowledge of adjacent visible faces
generateFromFace' :: V.Vector Int -> (Int, Int) -> (([Ind], [VertexD]), Int)
generateFromFace' arr (n', ind)
-- jerk : n = 0, undex shift now is in the function one level above
  | orient > 0 = ( ([V3 (4*n) (4*n+1) (4*n+2), V3 (4*n) (4*n+2) (4*n+3)], verts ), orient)  
  | otherwise = ( ([V3 (4*n) (4*n+2) (4*n+1), V3 (4*n) (4*n+3) (4*n+2)], verts ), -orient)
  where
    (V3 x y z, m) = unpack' ind
    -- orientation of a face is given in the faces array
    orient = arr V.! ind
    -- faces adjacent to a vertex
    adjFaces :: Int3 -> [F3]
    adjFaces v = map (\ k -> (fromIntegral $ faceValue k) * (normList !! (snd k))) (filter (\ k -> faceValue k /= 0) adjFacesList)
      where faceValue = \ k -> let (V3 a b c, d) = (fst k + v, snd k) in arr V.! (pack' a b c d)
    scaleCoords vect = floatize vect
      where floatize (V3 a b c) = (V3 (fromIntegral a) (fromIntegral b)  (fromIntegral (c - (quot chunkHeight 2))))
    -- edges adjacent to a vertex  
    adjEdges :: Int3 -> [F3]
    adjEdges v = concat $ map (\ k -> map (\ p -> (scaleCoords v) + p) (snd k)) (filter (\ k -> let (V3 a b c, d) =
                                                                                                      (fst (fst k) + v, snd (fst k)) in arr V.! (pack' a b c d) /= 0) $ zip adjFacesList adjEdgesList)
    vert' :: (Int3, F2) -> VertexD
    -- each vertex is computed from the adjacent Edges (position) and Faces (normals)
    vert' (v, tex) = VertexD ((sum $ adjEdges v)/(fromIntegral $ length $ adjEdges v)) (normalize $ sum $ adjFaces v) tex
    n = 0 --fromIntegral n'
    verts
      | m == 0 = createVerts (0, 1, 2, 3)
      | m == 1 = createVerts (1, 5, 6, 2)
      | otherwise = createVerts (0, 4, 5, 1)
    createVerts (n0, n1, n2, n3) = map vert' [((cubeVerts n0), V2 0.0 1.0), ((cubeVerts n1), V2 0.0 0.0), ((cubeVerts n2), V2 1.0 0.0), ((cubeVerts n3), V2 1.0 1.0)]
  
    cubeVerts n1 = (V3 x y z) + ([V3 0 1 0, V3 0 0 0, V3 1 0 0, V3 1 1 0, V3 0 1 (-1), V3 0 0 (-1), V3 1 0 (-1)] !! n1)
    normList :: [F3]
    normList = [V3 0.0 0.0 1.0, V3 0.0 (-1.0) 0.0, V3 (-1.0) 0.0 0.0]
    -- there are 12 possile faces adjacent to a vertex coming from one of 7 possible blocks
    adjFacesList :: [(Int3, Int)]
    adjFacesList = [(V3 0 0 0, 0), (V3 0 0 0, 1), (V3 0 0 0, 2), (V3 0 0 1, 1), (V3 0 0 1, 2), (V3 (-1) 0 0, 0), (V3 (-1) 0 0, 1), (V3 0 (-1) 0, 0),
                    (V3 0 (-1) 0, 2), (V3 (-1) (-1) 0, 0), (V3 (-1) 0 1, 1), (V3 0 (-1) 1, 2)]
    -- list of 12*2 adjacent edges corresponding to the list above
    adjEdgesList :: [[F3]]
    adjEdgesList = [[V3 0.0 1.0 0.0, V3 1.0 0.0 0.0], [V3 1.0 0.0 0.0, V3 0.0 0.0 (-1.0)], [V3 0.0 1.0 0.0, V3 0.0 0.0 (-1.0)], [V3 1.0 0.0 0.0, V3 0.0 0.0 (1.0)],
                    [V3 0.0 1.0 0.0, V3 0.0 0.0 (1.0)], [V3 0.0 1.0 0.0, V3 (-1.0) 0.0 (0.0)], [V3 0.0 1.0 0.0, V3 0.0 0.0 (-1.0)], [V3 1.0 0.0 0.0, V3 0.0 (-1.0) (0.0)],
                    [V3 0.0 (-1.0) 0.0, V3 0.0 0.0 (-1.0)], [V3 (-1.0) 0.0 0.0, V3 0.0 (-1.0) (0.0)], [V3 (-1.0) 0.0 0.0, V3 0.0 0.0 (1.0)], [V3 0.0 (-1.0) 0.0, V3 0.0 0.0 (1.0)]]


-- divides faces to meshes wrt textures
generateMeshFromFaces' :: VisibleFaces' -> [(([Ind], [VertexD]), Int)]
generateMeshFromFaces' (faces, inds) =  map snd $ M.toList $ concat' (M.empty)  (map (genFace) (zip ([0..] :: [Int]) inds)) --join' $ map (genFace) (zip ([0..] :: [Int]) inds)
  where
    genFace = \ x -> generateFromFace' faces x 
    concat' :: M.Map Int (([Ind], [VertexD]), Int) -> [(([Ind], [VertexD]), Int)] -> M.Map Int (([Ind], [VertexD]), Int)
    concat' x [] = x
    concat' x (f:fs) = concat' (concat'' x f) fs
    concat'' :: M.Map Int (([Ind], [VertexD]), Int) -> (([Ind], [VertexD]), Int) -> M.Map Int (([Ind], [VertexD]), Int)
    concat'' meshes f = case M.lookup (snd f) meshes of
      Nothing -> M.insert (snd f) f meshes
      Just msh -> let f' = ((map (\x -> x + (fromIntegral (2 * (length $ fst $ fst msh)))) (fst $ fst f), snd $ fst f), snd f)
                 in M.insert (snd f) (addFace' msh f') meshes
        where
          addFace' (xs,n) (x,_) = ((fst xs ++ (fst x), snd xs ++ (snd x)), n)
          
    --join' x = (concat $ fst (unzip x), concat $ snd (unzip x))
        
generateMeshFromFaces :: VisibleFaces' -> [(Mesh, Int)]
generateMeshFromFaces vf = map (\x -> (IM (IMesh (VS.fromList $ snd $ fst x) (VS.fromList $ fst $ fst x)), snd x)) dt
  where dt = generateMeshFromFaces' vf



  
type VisibleFaces' = (V.Vector Int, [Int])
visibleWidth :: Int
visibleWidth = chunkWidth + 4
visibleHeight :: Int
visibleHeight = 32

pack' :: Int -> Int -> Int -> Int -> Int
pack' a b c m = m + 3*(c + visibleHeight*(b' + visibleWidth*a'))
  where a' = a+2
        b' = b+2
unpack' :: Int -> (WorldPos, Int)
unpack' ind = (V3 a' b' (mod (div ind 3) visibleHeight), mod ind 3)
  where a' = (div ind (3*visibleHeight*visibleWidth))-2 
        b' = (mod (div ind (3*visibleHeight)) visibleWidth)-2


facesBlock' :: PosBlock -> Int3 -> [((Int, Int), Int)]
facesBlock' blocks v = if(blocks v > 0) then concatMap update' [0..5] else []
  where
    update' m = if(blocks (v + (fNorms m)) == 0) then [((num m, (fromIntegral $ blocks v)*(1 - 2*(quot m 3))), numFilter m)] else []
    v' m' = if m' >= 3 then (v + (fNorms m')) else v
    num m' = let (V3 a' b' c') = (v' m') in pack' a' b' c' (mod m' 3)
    numFilter m' = let (V3 a' b' _) = (v' m') in if((a'+1)*(b'+1)>0 && (16-a')*(16-b')>0) then num m' else -1
            
        
    fNorms n = (V.fromList ([V3 0 0 1, V3 0 (-1) 0, V3 (-1) 0 0, V3 0 0 (-1), V3 0 1 0, V3 1 0 0])) V.! n

  
blockBelowVisible :: PosBlock -> Int3 -> Bool
blockBelowVisible blocks (V3 x y z) = blocks (V3 x y (z-1)) > 0 && (blocks (V3 (x+1) y (z-1)) > 0 || blocks (V3 (x-1) y (z-1)) > 0 || blocks (V3 x (y+1) (z-1)) > 0 || blocks (V3 x (y-1) (z-1)) > 0)
  

generateVisibleFaces :: PosBlock -> VisibleFaces' -> VisibleFaces'
generateVisibleFaces blocks array@(arr, lst) = (arr V.// (map fst genColomns), filter (>(-1)) $ map snd genColomns)
  where
    genColomns = concatMap (\ n -> facesColomn' (v' n)) [0..(cw1*(cw1)-1)]
    cw1 = chunkWidth+2
    v' n = getClosestGround'' blocks (V3 (div n cw1 - 1) (mod n cw1 - 1) (chunkHeight-1))

    facesColomn' v@(V3 x y z)
      | z > 2 = if(blockBelowVisible blocks v) then  facesBlock' blocks v else facesColomn' (V3 x y (z-1))
      | otherwise = []


  
-- naive surfaces nets mesh generation method
sGenerateMeshFromBlocks :: PosBlock -> [(Mesh, Int)]
sGenerateMeshFromBlocks blocks = generateMeshFromFaces edgeFaces
  where 
   edgeFaces = generateVisibleFaces blocks $ (V.generate (3*(visibleWidth)*(visibleWidth)*visibleHeight) (\ _ -> 0), [])
    
          
generate3 :: Int -> Int -> Int -> (Int -> Int -> Int -> a) -> V.Vector a
generate3 sx sy sz fxyz = V.generate (sx*sy*sz) (\ x -> fxyz (div (div x sz) sy) (mod (div x sz) sy) (mod x sz))


noiseAmplitude :: Double
noiseLattice :: Double
noiseShift :: Double

noiseAmplitude  = 8.0
noiseLattice = 4.0
noiseShift = 2**10
  

  
-- creates a chunck using Perlin noise
generateChunk :: MapRandom -> HorizontalPos -> Chunk
generateChunk (NoiseRandom cs co csc cp, biomeRandom) chunkPos@(V2 x y) = (Chunk blocks' chunkPos) where
  blocks' = blocksFromHeightFunction(hFunction)
  blocksFromHeightFunction hFunction' = generate3 chunkWidth chunkWidth chunkHeight (\ xc yc zc -> if( zc > (hFunction' xc yc)) then 0 else fromIntegral $ biomeFunction xc yc)
  biomeFunction xc yc = getBiome biomeRandom $ V2 (x*chunkWidth + xc) (y*chunkWidth + yc)
  hFunction = (\ xc yc -> quot chunkHeight 2 + (round $ noiseAmplitude*(noiseValue perlinNoise ((fromIntegral $ x*chunkWidth + xc)/noiseLattice +
                                                                                                noiseShift, (fromIntegral $ y*chunkWidth + yc)/noiseLattice + noiseShift, 0.0))))
  -- test height function
  hFunctionTest = (\ xc yc -> if (xc == (8 :: Int) && yc == (8 :: Int)) then 17 :: Int else 16)
--  hmap = generate2 16 16 hFunction
  perlinNoise = perlin  cs co csc cp
