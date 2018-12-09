module Engine.Map
  ( Map(..)
  , MapBuffer
  , ChunkBuffer
  , getChunk
  , gBlock'
  , createChunkMeshes
  , createChunkMesh'
  , initChunkBuffers
  , initChunkMesh'
  , initMap
  , initMapBuffer
  , initChunks
  ) where

import Data.Vector (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Maybe
import qualified Data.Map.Strict as M
import qualified Control.Monad as Mon
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Graphics.Caramia

import Linear.V2
import Linear.V3


import Engine.Types
import Engine.Mesh
import Engine.Chunk

import Debug.Trace

-- base map data structure
data Map = Map { chunks :: M.Map HorizontalPos (Chunk, Maybe Mesh)
               , mapRnd :: ChunkRandom
               }

type MapBuffer = (M.Map HorizontalPos ChunkBuffer, M.Map ByteString Texture)

type ChunkBuffer = [(MeshBuffer, [ByteString])]



---------------------------------------------------------------  
------------------------ GAME LOGIC----------------------------  
---------------------------------------------------------------  

  
-- functions
-- creates new chunk if there is no one
getChunk' :: Map -> HorizontalPos -> (Map, Chunk)
getChunk' mp@(Map {..}) (flip M.lookup chunks -> Just (chunk, m)) = (mp, chunk)
getChunk' mp@(Map {..}) pos = getChunk' (newChunk mp pos) pos

getChunk :: Map -> HorizontalPos -> Maybe Chunk
getChunk mp@(Map {..}) pos = fmap fst $ (M.lookup pos chunks)


getChunkUnsafe :: Map -> HorizontalPos -> Chunk
getChunkUnsafe mp@(Map {..}) (flip M.lookup chunks -> Just (chunk, m)) = chunk
getChunkUnsafe _ pos = error $ "Non-existing chunk at " Prelude.++ (show pos)
  
newChunk :: Map -> HorizontalPos -> Map
newChunk mp@(Map {..}) (flip M.lookup chunks -> Just _) = mp
newChunk (Map {..}) pos = mp'
  where
    mp' = Map chunks' mapRnd
    chunks' = M.insert pos (generateChunk mapRnd pos, Nothing) chunks

  
gBlock' :: Map -> WorldPos -> (Map, Block)
gBlock' mp (V3 x y z) = gBl $ getChunk' mp (V2 (div x chunkWidth) (div y chunkWidth))
  where
    gBl (mp', ch) = (mp', getBlock' ch (V3 (mod x chunkWidth) (mod y chunkWidth) z))

gBlock :: Map -> WorldPos -> Maybe Block
gBlock mp (V3 x y z) = getBlock'' (getChunk mp (V2 (div x chunkWidth) (div y chunkWidth))) (V3 (mod x chunkWidth) (mod y chunkWidth) z)
  where
    getBlock'' ch pos = fmap (\ x -> getBlock' x pos) ch

gBlockUnsafe :: Map -> WorldPos -> Block
gBlockUnsafe mp (V3 x y z) = getBlock' (getChunkUnsafe mp (V2 (div x chunkWidth) (div y chunkWidth))) (V3 (mod x chunkWidth) (mod y chunkWidth) z)


gBlockUnsafe' :: Map -> HorizontalPos -> WorldPos -> Block
gBlockUnsafe' mp (V2 xh yh) (V3 x y z) = getBlock' (getChunkUnsafe mp (V2 (div (x) chunkWidth + xh) (div (y) chunkWidth + yh))) (V3 (mod x chunkWidth) (mod y chunkWidth) z)

---------------------------------------------------------------  
------------------------ SYSTEM -------------------------------  
---------------------------------------------------------------

-- TODO: normal textures
addChunkBuffer :: HorizontalPos -> MeshBuffer -> MapBuffer -> MapBuffer
addChunkBuffer pos cBuff (cBuffs, texs') = ( M.insert pos [(cBuff, ["grass1"])] cBuffs, texs') 

-- creates meshes for a new map
initMap :: ChunkRandom -> [HorizontalPos] -> Map
initMap rnd posns = createChunkMeshes (Prelude.foldl newChunk newMp posns) posns
  where
    newMp = Map M.empty rnd

-- loads textures for the map
initMapBuffer :: FilePath -> [FilePath] -> IO MapBuffer
initMapBuffer path names = do
  textures <- mapM loadTex fullNames
  let textures' = catMaybes textures
  return (M.empty, M.fromList $ zip (map pack names) textures')
  where
    fullNames = map (\x -> path ++ x ++ ".bmp") names

createChunkMeshes :: Map -> [HorizontalPos] -> Map
createChunkMeshes mp posns = Prelude.foldl createChunkMesh' mp posns
createChunkMesh' :: Map -> HorizontalPos -> Map
createChunkMesh' mp@(Map chnks rnd) pos = (Map chunks' rnd)
  where
    chunks' = case M.lookup pos chnks of
      Just (chunk, _) -> M.insert pos (chunk, Just $ sGenerateMeshFromBlocks (gBlockUnsafe' mp (V2 x y))) chunks''
      Nothing -> chnks
    (V2 x y) = pos
    --depends on the method!
    (Map chunks'' _) = mp--newSurroundingChunks mp pos
--    newSurroundingChunks :: Map -> HorizontalPos -> Map
--    newSurroundingChunks mp' (V2 x' y') = (newChunk (newChunk (newChunk (newChunk (newChunk mp' $ V2 (x'-1) (y'-1) ) $ V2 x' (y'-1) ) $ V2 x' (y'+1) ) $ V2 (x'-1) y' ) $ V2 (x'+1) y')



initChunkBuffers :: Map -> [HorizontalPos] -> MapBuffer -> IO MapBuffer
initChunkBuffers mp posns buff = Mon.foldM (\ x y -> initChunkMesh' mp y x) buff posns
initChunkMesh' :: Map -> HorizontalPos -> MapBuffer -> IO MapBuffer
initChunkMesh' (Map {..}) pos buff
  | not (isNothing (M.lookup pos (fst buff))) = return buff
  | otherwise = case(M.lookup pos chunks) of
    Nothing ->  error $ "No chunk at " Prelude.++ (show pos) --return buffs
    Just ch -> case snd ch of
      Nothing -> error $ "No chunck mesh at " Prelude.++ (show pos) --return buffs
      Just mesh -> do
        chunkBuff <- initMeshBuffer mesh
        return $ addChunkBuffer pos chunkBuff buff

initChunks :: [HorizontalPos] -> (Map, MapBuffer) -> IO (Map, MapBuffer)
initChunks posns (mp, buff) = fmap (\ x -> (mp', x)) buff'
  where
    buff' = initChunkBuffers mp' posns buff
    mp' = createChunkMeshes (Prelude.foldl newChunk mp posns') posns'
    posns' = concat $ map (\ x -> [x, x+(V2 0 1), x+(V2 1 0), x+(V2 (-1) 0), x+(V2 0 (-1)), x+(V2 (-1) (-1)), x+(V2 (-1) (1)), x+(V2 (1) (-1)), x+(V2 (1) (1))]) posns 

  
