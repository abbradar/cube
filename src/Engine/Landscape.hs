module Engine.Landscape
  ( Landscape
  ) where


import Data.Word
import Linear
import GHC.Generics (Generic(..))
import Foreign.Storable (Storable(..))
import Foreign.Storable.Generic
import Foreign.Storable.Tuple ()
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple ()


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

-- now just a chunk

type Block = Word8

type IArray2 = Vector ( Vector Block )
type IArray3 = Vector IArray2

  
getBlock :: IArray3 -> WorldPos -> Block
getBlock arr (V3 a b c) = undefined -- arr VS.! a  VS.! b  VS.! c
  
data Landscape = Landscape { blocks :: IArray3
                           , lmesh :: Mesh
                           , pos :: HorizontalPos
                           } 
  
isEmpty :: Landscape -> WorldPos -> Bool
isEmpty (Landscape bl _ _) pos = getBlock bl pos == 0 

-- if the input block is solid returns itself
getClosestGround :: Landscape -> WorldPos -> WorldPos
getClosestGround lsc@(Landscape bl _ _) (V3 a b c) = (V3 a b (findBottom c))
  where
    findBottom 1 = -1
    findBottom z = if(isEmpty lsc (V3 a b z)) then findBottom (z-1) else z

  
