module Engine.Map
  ( Map(..)
  , MapBuffer
  , ChunkBuffer
  , getChunk
  , getClosestGroundMap
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
data Map = Map { chunks :: M.Map HorizontalPos (Chunk, [(Mesh, Int)])
               -- chunk random + biome random
               , mapRnd :: MapRandom
               }

type MapBuffer = (M.Map HorizontalPos ChunkBuffer, M.Map ByteString Texture)

-- Maybe MeshBuffer
type ChunkBuffer = [(Maybe MeshBuffer, [ByteString])]



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


getClosestGroundMap :: Map -> F3 -> F3
getClosestGroundMap mp pos@(V3 x y z) = V3 x y (fromIntegral z1)
  where
    (V3 _ _ z1) = getClosestGround chck $ (fromIntegral <$> (V3 0 0 (floor z))) + ((\x -> mod x chunkWidth) <$> floor <$> (V3 x y 0))
    chck = fromMaybe (error "chunk is not initialized") $ getChunk mp ((\x -> div x chunkWidth) <$> (floor <$> (V2 x y)))

  
newChunk :: Map -> HorizontalPos -> Map
newChunk mp@(Map {..}) (flip M.lookup chunks -> Just _) = mp
newChunk (Map {..}) pos = mp'
  where
    mp' = Map chunks' mapRnd
    chunks' = M.insert pos (generateChunk mapRnd pos, []) chunks

  
-- adds a new chunk if the block is out of bounds of the current chunks  
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
gBlockUnsafe' mp (V2 xh yh) (V3 x y z) = {-# SCC "justSoGAY" #-}  getBlock' (getChunkUnsafe mp (V2 (div (x) chunkWidth + xh) (div (y) chunkWidth + yh))) (V3 (mod x chunkWidth) (mod y chunkWidth) z)

---------------------------------------------------------------  
------------------------ SYSTEM -------------------------------  
---------------------------------------------------------------

-- TODO: normal textures
addChunkBuffers :: HorizontalPos -> [(MeshBuffer, ByteString)] -> MapBuffer -> MapBuffer
addChunkBuffers pos cBuff (cBuffs, texs') = ( M.insert pos cBuff' cBuffs, texs')
  where cBuff' = map (\(x,y) -> (Just x, [y])) cBuff

-- creates meshes for a new map
initMap :: MapRandom -> [HorizontalPos] -> Map
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
      Just (chunk, _) -> M.insert pos (chunk, sGenerateMeshFromBlocks (gBlockUnsafe' mp (V2 x y))) chnks
      Nothing -> chnks
    (V2 x y) = pos


-- TODO: make something better then this
textureNamesList :: [ByteString]
textureNamesList = ["sand", "grass1", "grass3"]


initChunkBuffers :: Map -> [HorizontalPos] -> MapBuffer -> IO MapBuffer
initChunkBuffers mp posns buff = Mon.foldM (\ x y -> initChunkMesh' mp y x) buff posns
initChunkMesh' :: Map -> HorizontalPos -> MapBuffer -> IO MapBuffer
initChunkMesh' (Map {..}) pos buff
  | not (isNothing (M.lookup pos (fst buff))) = return buff
  | otherwise = case(M.lookup pos chunks) of
    Nothing ->  error $ "No chunk at " Prelude.++ (show pos) --return buffs
    Just ch -> case snd ch of
      [] -> error $ "No chunk mesh at " Prelude.++ (show pos) --return buffs
      meshes -> do
        chunkBuffs <- mapM initMeshBuffer $ map fst meshes
        return $ addChunkBuffers pos (zip chunkBuffs (map (\x -> textureNamesList !! (snd x)) meshes)) buff

initChunks :: [HorizontalPos] -> (Map, MapBuffer) -> IO (Map, MapBuffer)
initChunks posns (mp, buff) = fmap (\ x -> (mp', x)) buff'
  where
    buff' = initChunkBuffers mp' posns buff
    mp' = createChunkMeshes (Prelude.foldl newChunk mp posns') posns'
    posns' = concat $ map (\ x -> [x, x+(V2 0 1), x+(V2 1 0), x+(V2 (-1) 0), x+(V2 0 (-1)), x+(V2 (-1) (-1)), x+(V2 (-1) (1)), x+(V2 (1) (-1)), x+(V2 (1) (1))]) posns 

  
