-- |

module Cube.Graphics.Animation where

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import Cube.Graphics.Types
import qualified Data.GlTF.Types as TF
import qualified Data.GlTF.Resources as TF
import qualified Data.GlTF.Nodes as TF

data LoadedSamplerOutput = LSOTranslation (VS.Vector V3F)
                         | LSORotation (VS.Vector QF)
                         | LSOScale (VS.Vector V3F)
                         | LSOWeights (Vector (VS.Vector V3F))
                         deriving (Show, Eq)

data LoadedSampler = LoadedSampler { lsampInputs :: Vector Float
                                   , lsampOutputs :: LoadedSamplerOutput
                                   }

newtype LoadedAnimation = LoadedAnimation { lanimSamplers :: HashMap TF.NodeIndex LoadedSampler
                                          }
