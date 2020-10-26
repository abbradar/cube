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
                         , nodeTreeChildren :: NodeChildren
                         }
              deriving (Show)

gltfNodeTree :: Vector Node -> Either String NodeChildren
gltfNodeTree nodes = getChildren S.empty $ filter (not . (`S.member` childNodes)) [0..V.length nodes - 1]
  where childNodes = S.fromList $ concatMap (V.toList . fromMaybe V.empty . nodeChildren) $ V.toList nodes

        getChildren visited children = V.fromList <$> mapM (getNode visited) children
        getNode visited nodeIndex
          | nodeIndex `S.member` visited = Left "Cycle detected in nodes"
          | otherwise = do
            when (nodeIndex < 0 || nodeIndex >= V.length nodes) $ Left [i|Node index #{nodeIndex} not found|]
            let nodeTreeNode = nodes V.! nodeIndex
                newVisited = S.insert nodeIndex visited
            nodeTreeChildren <- getChildren newVisited $ V.toList $ fromMaybe V.empty $ nodeChildren nodeTreeNode
            return NodeTree {..}
