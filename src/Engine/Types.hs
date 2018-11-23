module Engine.Types where

import Data.Word
import Linear
import GHC.Generics (Generic(..))
import Foreign.Storable (Storable(..))
import Foreign.Storable.Generic
import Foreign.Storable.Tuple ()
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import Foreign.Storable.Tuple ()



---------------------------------------------------------
------------------ COMMON TYPES ----------------------
---------------------------------------------------------

type F2 = V2 Float
type F3 = V3 Float
type F4 = V4 Float
type I3 = V3 Word16
type I4 = V4 Word16
type MF44 = M44 Float
type QF = Quaternion Float

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * VS.length vec

---------------------------------------------------------
------------------ GRAPHICAL TYPES ----------------------
---------------------------------------------------------
type Vert = V2 F3
type Ind = I3
type VSkinData = (V4 Word8, V4 Float)


  
-- vertex with coords, normals and texture coords, the default one
data VertexD = VertexD { position :: !F3
                       , normal :: !F3
                       , texcoords :: !F2
                       }
             deriving (Generic, Show, Eq, Read)

-- ordinary vertex
instance Storable VertexD where
  peek = gPeek
  poke = gPoke
  sizeOf = gSizeOf
  alignment = gAlignment


-- skinned vertex
data SVertexD = SVertexD { sposition :: !F3
                         , snormal :: !F3
                         , stexcoords :: !F2
                         , bones :: !VSkinData
                         }
             deriving (Generic, Show, Eq, Read)


instance Storable SVertexD where
  peek = gPeek
  poke = gPoke
  sizeOf = gSizeOf
  alignment = gAlignment


---------------------------------------------------------
------------------ GAME TYPES ----------------------
---------------------------------------------------------

type WorldPos = V3 Int
type HorizontalPos = V2 Int
