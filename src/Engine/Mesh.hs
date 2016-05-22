module Engine.Mesh
       ( Vert
       , Ind
       , Mesh
       , vertices
       , indices
       , loadMeshOBJ
       , loadMeshX
       , MeshBuffer
       , initMeshBuffer
       , drawMesh
       ) where

import Foreign.Storable (Storable(..))
import Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import qualified Data.Scientific as S
import Text.Trifecta.Parser (parseFromFile)
import Graphics.Caramia
import Linear.V2
import Linear.V3

import Data.Wavefront
import Data.DirectX
import Engine.Types

--import Debug.Trace

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * VS.length vec

type Vert = V2 F3
type Ind = I3

data Mesh = Mesh { vertices :: Vector Vert
                 , indices :: Vector Ind
                 } 

loadMeshOBJ :: FilePath -> IO Mesh
loadMeshOBJ name = do
  values <- parseFromFile wavefrontOBJ name
  case values of
    Nothing -> fail "loadMeshOBJ: failed to load mesh"
    Just r -> do
      let WFModel{..} = extractModel r
          indlist = map (fmap fromIntegral) wfIndices
          --TODO: check different length
          convertF = map (fmap S.toRealFloat)
          vertlist = zipWith V2 (convertF wfVertices) (convertF wfNormals)
          --vertlist = zip wfVertices wfNormals
      return Mesh { vertices = VS.fromList vertlist
                  , indices = VS.fromList indlist
                  }

loadMeshX :: XTemplates -> FilePath -> IO Mesh
loadMeshX tmpls name = undefined

data MeshBuffer = MeshBuffer { mvao :: VAO
                             , mbuffer :: Buffer
                             , indOffset :: Int
                             , indNumber :: Int
                             }

initMeshBuffer :: Mesh -> IO MeshBuffer
initMeshBuffer mesh = do
  vao <- newVAO
  buff <- newBuffer defaultBufferCreation { accessHints = (Static, Draw)
                                          , size = sizeOfV (vertices mesh) + sizeOfV(indices mesh)
                                          }
  uploadVector (vertices mesh) 0 buff
  sourceVertexData buff defaultSourcing { components = 3
                                        , stride = sizeOf (undefined :: Vert)
                                        , attributeIndex = 0
                                        , sourceType = SFloat
                                        } vao
  
  sourceVertexData buff defaultSourcing { offset = sizeOf (undefined :: (V3 Float))
                                        , components = 3
                                        , stride = sizeOf (undefined :: Vert)
                                        , attributeIndex = 1
                                        , sourceType = SFloat
                                        } vao

  uploadVector (indices mesh) (sizeOfV (vertices mesh)) buff
  return MeshBuffer { mvao = vao
                    , mbuffer = buff
                    , indOffset = sizeOfV (vertices mesh)
                    , indNumber = 3*(VS.length (indices mesh))
                    }

drawMesh :: MeshBuffer -> Pipeline -> DrawT IO ()
drawMesh mesh pl =
  drawR drawCommand { primitiveType = Triangles
                    , primitivesVAO = mvao mesh
                    , numIndices = indNumber mesh
                    -- , sourceData = Primitives 0
                    , sourceData = PrimitivesWithIndices (mbuffer mesh) (indOffset mesh) IWord16
                    }
