-- | Convert nodes from GlTF into a tree.

{-# LANGUAGE StrictData #-}

module Data.GlTF.Nodes
       ( NodeTree(..)
       , NodeChildren
       , gltfNodeTree
       ) where

import Control.Monad
import Data.Maybe
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.String.Interpolate

import Data.GlTF.Types

type NodeChildren = Vector NodeTree

data NodeTree = NodeTree { nodeTreeNode :: Node
                         , nodeTreeIndex :: NodeIndex
                         , nodeTreeChildren :: NodeChildren
                         }
              deriving (Show)

gltfNodeTree :: Vector Node -> Either String NodeChildren
gltfNodeTree nodes = getChildren S.empty $ filter (not . (`S.member` childNodes)) [0..V.length nodes - 1]
  where childNodes = S.fromList $ concatMap (V.toList . fromMaybe V.empty . nodeChildren) $ V.toList nodes

        getChildren visited children = V.fromList <$> mapM (getNode visited) children
        getNode visited nodeTreeIndex
          | nodeTreeIndex `S.member` visited = Left "Cycle detected in nodes"
          | otherwise = do
            when (nodeTreeIndex < 0 || nodeTreeIndex >= V.length nodes) $ Left [i|Node index #{nodeTreeIndex} not found|]
            let nodeTreeNode = nodes V.! nodeTreeIndex
                newVisited = S.insert nodeTreeIndex visited
            nodeTreeChildren <- getChildren newVisited $ V.toList $ fromMaybe V.empty $ nodeChildren nodeTreeNode
            return NodeTree {..}
