module Data.Wavefront
       ( WFValue(..)
       , parseOBJ
       , WFModel(..)
       , extractModel
       ) where

import Data.Default

import Data.Wavefront.Parser

data WFModel = WFModel { wfNormals :: [F3]
                       , wfVertices :: [F3]
                       , wfIndices :: [I3]
                       }
             deriving (Show, Eq)

-- FIXME: replace when data-default-class is updated on Hackage
instance Default WFModel where
  def = WFModel { wfNormals = []
                , wfVertices = []
                , wfIndices = []
                }

extractModel :: [WFValue] -> WFModel
extractModel [] = def
extractModel (h:t) = case h of
  WFVertex a -> m { wfVertices = a : wfVertices }
  WFNormal a -> m { wfNormals = a : wfNormals }
  WFFace a -> m { wfIndices = a : wfIndices }
  where m@(WFModel {..}) = extractModel t
