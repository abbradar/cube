-- | world map consists of chunks

module Cube.Map
  ( Chunk(..)
  , Map(..)
  , ChunkLRU(..)
  , getChunk
  , getLoadedChunks
  , Movable(..)
  , chunkBuffers
  , chunkWidth
  , chunkHeight
  , generateChunk
  , MapRandom(..)
  , MapData(..)
  , mapFromRnd
  ) where

import qualified Data.Vector as V
import Data.Word
import qualified Data.Vector.Storable as VS
import qualified Data.ByteString as B
import qualified Data.Vector.Storable.ByteString as VSB
import qualified Data.Map as M
import Data.Maybe
import Linear
import Data.Int
import Foreign.C.Types
import Numeric.Noise.Perlin

import Data.Aeson
import Data.Aeson.Utils
import GHC.Generics (Generic)


-- types

type Block = Word8
type BlockIdx = Word8

-- TODO load some of the constants

chunkWidth :: Int
chunkHeight :: Int
chunkWidth = 16
chunkHeight = 32

data Chunk = Chunk { chunkBlocks :: VS.Vector BlockIdx
                   , chunkPosition :: V2 Int
                   } deriving (Show, Eq, Read)

data ChunkLRU = ChunkLRU { lruChunks :: V.Vector Chunk
                         , lruMap :: M.Map (V2 Int) Int
                         } deriving (Show, Eq, Read)

data Map = Map { mapChunks :: ChunkLRU
             --  , mapActiveRadius :: Int
             --  , mapLoadRadius :: Int
               , mapData :: MapData
               } deriving (Show, Eq, Read)

getChunk :: Map -> V2 Int -> Maybe Chunk
getChunk Map{..} pos = fmap (lruChunks mapChunks V.!) (M.lookup pos (lruMap mapChunks))

getLoadedChunks :: Map -> V.Vector (V2 Int)
getLoadedChunks Map{..} = V.fromList $ map fst $ M.toList $ lruMap mapChunks

class Movable a where
  moveRotate :: a -> V3 Float -> Quaternion Float -> Map -> a

-- logic

pck3 :: V3 Int -> Int
pck3 (V3 a b c) = c + chunkHeight*(b + chunkWidth*a)

pck3' :: V3 Int -> Int -> Int -> Int
pck3' (V3 a b c) w h = c + h*(b + w*a)


upck3 :: Int -> V3 Int
upck3 x = V3 (quot x $ chunkHeight * chunkWidth) (mod (quot x chunkHeight) chunkWidth) (mod x chunkHeight)

upck3' :: Int -> Int -> Int -> V3 Int
upck3' x w h = V3 (quot x $ h * w) (mod (quot x h) w) (mod x h)


getBlock :: Chunk -> V3 Int -> BlockIdx
getBlock (Chunk blocks _) pos = blocks VS.! pck3 pos

isEmptyS :: Chunk -> V3 Int -> Bool
isEmptyS (Chunk blocks _) pos@(V3 a b c) = if a >= chunkWidth || b >= chunkWidth || c >= chunkHeight then error (show pos) else blocks VS.! pck3 pos == 0


isEmpty :: Chunk -> V3 Int -> Bool
isEmpty (Chunk blocks _) pos@(V3 a b c) = blocks VS.! pck3 pos == 0

isTransparent :: Chunk -> V3 Int -> Bool
isTransparent (Chunk blocks _) pos = blocks VS.! pck3 pos == 0

closestGround :: Chunk -> V3 Int -> V3 Int
closestGround ch@(Chunk blocks _) (V3 a b c) = findBottom c
  where
    findBottom 1 = 1
    findBottom z = if isEmpty ch (V3 a b z) then findBottom (z-1) else V3 a b z

-- mesh generation

chunkSize :: Int
chunkSize = chunkWidth^2 * chunkHeight

-- minecraft style chunk mesh
--chunkBuffers :: Chunk ->
chunkBuffers :: Chunk -> Chunk -> Chunk -> (B.ByteString, B.ByteString, Int)
chunkBuffers chunk chunkX chunkY = (vbuffer, ibuffer, 0)
  where
    vbuffer = B.concat [pbuff, nbuff, uvbuff]
    pbuff = VSB.vectorToByteString $ VS.map faceToPos vFacesVect
    nbuff = VSB.vectorToByteString $ VS.map faceToNorms vFacesVect
    uvbuff = VSB.vectorToByteString $ VS.replicate (VS.length vFacesVect) uvs'
    ibuffer = VSB.vectorToByteString $ VS.imap faceToInds vFacesVect

    -- run over inner blocks
    vFacesList' = concatMap (\x -> visibleFacesXYZ (V3 1 1 1 + upck3' x (chunkWidth-1) (chunkHeight-1)) [(chunk, V3 (-1) 0 0), (chunk, V3 0 (-1) 0), (chunk, V3 0 0 (-1))]) [0..(chunkWidth-1)^2*(chunkHeight-1)-1]
    vFacesListX = concatMap (\x -> visibleFacesXYZ (V3 0 (1+mod x (chunkWidth-1)) (1+div x (chunkWidth-1))) [(chunkX, V3 15 0 0), (chunk, V3 0 (-1) 0), (chunk, V3 0 0 (-1))]) [0..(chunkWidth-1)*(chunkHeight-1)-1]
    vFacesListY = concatMap (\x -> visibleFacesXYZ (V3 (1+mod x (chunkWidth-1)) 0 (1+div x (chunkWidth-1))) [(chunk, V3 (-1) 0 0), (chunkY, V3 0 15 0), (chunk, V3 0 0 (-1))]) [0..(chunkWidth-1)*(chunkHeight-1)-1]
    vFacesListXY = concatMap (\x -> visibleFacesXYZ (V3 0 0 (1+x)) [(chunkX, V3 15 0 0), (chunkY, V3 0 15 0), (chunk, V3 0 0 (-1))]) [0..(chunkHeight-1)-1]
    vFacesList = vFacesList' ++ vFacesListX ++ vFacesListY ++ vFacesListXY
    vFacesVect = VS.fromList vFacesList
    -- for every block check adjacent blocks and add a face if exactly one of them is transparent
    visibleFacesXYZ pos chunksShifts = lst
    --(chkX, shiftX) (chkY, shiftY) (chkZ, shiftZ) = lst'
      where
        xor True False = True
        xor False True = True
        xor _ _ = False
        isEmp = isEmpty chunk pos
        lst = mapMaybe (\((x,y),z) -> if xor isEmp (isEmpty x $ pos + y) then Just $ V2 (3*pck3 pos + z) (if isEmp then (-1) else 1) else Nothing) (zip chunksShifts [0,1,2])
        --lst = (if xor (isEmp) (isEmpty chkX $ pos + shiftX) then [V2 (3*pck3 pos+0) (if isEmp then (-1) else 1)] else []) ++
        --      (if xor (isEmp) (isEmpty chkY $ pos + shiftY) then [V2 (3*pck3 pos+1) (if isEmp then (-1) else 1)] else []) ++
        --      (if xor (isEmp) (isEmpty chkZ $ pos + shiftZ) then [V2 (3*pck3 pos+2) (if isEmp then (-1) else 1)] else [])


    -- normals corresponding to the three orientations of the face
    norms = V.fromList [V3 1.0 0.0 0.0, V3 0.0 1.0 0.0, V3 0.0 0.0 1.0]
    -- generate vertices for a given face
    posShifts = V.fromList [V.fromList [V3 0.0 0.0 0.0, V3 0.0 1.0 0.0, V3 0.0 1.0 1.0, V3 0.0 0.0 1.0], V.fromList [V3 0.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 0.0 1.0, V3 0.0 0.0 1.0], V.fromList [V3 0.0 0.0 0.0, V3 1.0 0.0 0.0, V3 1.0 1.0 0.0, V3 0.0 1.0 0.0]]
    faceToPos :: V2 Int -> V4 (V3 Float)
    faceToPos (V2 iplus _) = V4 pos (pos + posShifts V.! i V.! 1)  (pos + posShifts V.! i V.! 2) (pos + posShifts V.! i V.! 3)
      where i = iplus `mod` 3
            pos = fmap fromIntegral $ upck3 $ iplus `div` 3
    faceToNorms :: V2 Int -> V4 (V3 Float)
    faceToNorms (V2 iplus or') = case or' of
      1 -> V4 (-norms V.! i) (-norms V.! i) (-norms V.! i) (-norms V.! i)
      otherwise -> V4 (norms V.! i) (norms V.! i) (norms V.! i) (norms V.! i)
      where i = iplus `mod` 3
    faceToInds :: Int -> (V2 Int) -> V2 (V3 Int16)
    faceToInds i (V2 _ or') = case or' of
      1 -> fmap (fmap fromIntegral) $ V2 (V3 (4*i) (4*i+1) (4*i+2)) (V3 (4*i) (4*i+2) (4*i+3))
      otherwise -> fmap (fmap fromIntegral) $ V2 (V3 (4*i) (4*i+2) (4*i+1)) (V3 (4*i) (4*i+3) (4*i+2))
    uvs' :: V4 (V2 CFloat)
    uvs' = V4 (V2 0.0 0.0) (V2 0.0 1.0) (V2 1.0 1.0) (V2 1.0 0.0)

-- TODO specialize to make faster
getMapBlock :: ChunkLRU -> V3 Int -> Block
getMapBlock ChunkLRU{..} (V3 x y z) = chBlocks VS.! pck3 (V3 x' y' z)
  where chBlocks = chunkBlocks chunk
        x' = mod x chunkWidth
        y' = mod y chunkWidth
        chunk = lruChunks V.! (lruMap M.! V2 (quot x chunkWidth) (quot y chunkWidth))



-- chunk generation

--data NoiseParameters = NoiseParameters { noiseAmplitude :: Double
--                                       , noiseLattice :: Double
--                                       , noiseShift :: Double
--                                       }

data MapRandom = MapRandom { randomSeed :: Int
                           , randomOctaves :: Int
                           , randomScale :: Double
                           , randomPersistance :: Double
                           , randomAmplitude :: Double
                           , randomLattice :: Double
                           , randomShift :: Double
                           } deriving (Show, Read, Eq, Generic)

mapJSONOptions :: String -> Options
mapJSONOptions prefix = defaultOptions { omitNothingFields = False
                                     , fieldLabelModifier = removePrefix prefix
                                     }


instance FromJSON MapRandom where
  parseJSON = genericParseJSON $ mapJSONOptions "random"

data MapData = MapData { mapRandom :: MapRandom
                       , mapPath :: FilePath
                       } deriving (Show, Read, Eq, Generic)

instance FromJSON MapData where
  parseJSON = genericParseJSON $ mapJSONOptions "map"


--noiseParameters :: NoiseParameters
--noiseParameters = NoiseParameters{ noiseAmplitude = 8.0, noiseLattice = 4.0, noiseShift = 2**10 }

getBiome :: MapRandom -> V2 Int -> Int
getBiome (MapRandom ns no nsc np _ noiseLattice noiseShift) (V2 x y) = if (f > 0) then 1 else 2
  where
    f = noiseValue perlinNoise ((fromIntegral $ x)/noiseLattice + noiseShift, (fromIntegral $ y)/noiseLattice + noiseShift, 0.0)
    perlinNoise = perlin ns no nsc np

generate3 :: VS.Storable a => Int -> Int -> Int -> (Int -> Int -> Int -> a) -> VS.Vector a
generate3 sx sy sz fxyz = VS.generate (sx*sy*sz) (\ x -> fxyz (div (div x sz) sy) (mod (div x sz) sy) (mod x sz))

-- creates a chunck using Perlin noise
generateChunk :: MapRandom -> V2 Int -> Chunk
generateChunk mr@(MapRandom ns no nsc np noiseAmplitude noiseLattice noiseShift) chunkPos@(V2 x y) = (Chunk blocks' chunkPos) where
  blocks' = blocksFromHeightFunction(hFunction)
  blocksFromHeightFunction hFunction' = generate3 chunkWidth chunkWidth chunkHeight (\ xc yc zc -> if( zc > (hFunction' xc yc)) then 0 else fromIntegral $ biomeFunction xc yc)
  biomeFunction xc yc = getBiome mr $ V2 (x*chunkWidth + xc) (y*chunkWidth + yc)
  hFunction = (\ xc yc -> quot chunkHeight 2 + (round $ noiseAmplitude*(noiseValue perlinNoise ((fromIntegral $ x*chunkWidth + xc)/noiseLattice +
                                                                                                noiseShift, (fromIntegral $ y*chunkWidth + yc)/noiseLattice + noiseShift, 0.0))))
  -- test height function
  hFunctionTest = (\ xc yc -> if (xc == (8 :: Int) && yc == (8 :: Int)) then 17 :: Int else 16)
--  hmap = generate2 16 16 hFunction
  perlinNoise = perlin  ns no nsc np

mapFromRnd :: MapData -> [V2 Int] -> Map
mapFromRnd md lst = Map{ mapChunks = chunks, mapData = md }
  where
    chunksVector = V.fromList $ map (generateChunk $ mapRandom md) lst
    chunksMap = M.fromList $ zip lst [0..]
    chunks = ChunkLRU{ lruChunks = chunksVector, lruMap = chunksMap }
