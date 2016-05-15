module Engine.Mesh where

import qualified Data.Vector.Storable as VS
import Data.Vector.Storable (Vector)
import Linear.V2
import Linear.V3
import Linear.Matrix
import Data.Wavefront
import qualified Data.ByteString.Lazy as BL
import Data.Attoparsec.ByteString.Lazy
import Graphics.Caramia
import Foreign.Storable (Storable(..))

--import Debug.Trace

-- Total size of a vector; maybe useful enough to move inside library
sizeOfV :: forall a. (Storable a) => Vector a -> Int
sizeOfV vec = sizeOf (undefined :: a) * VS.length vec

type Vert = V2 F3
type Ind = I3

data Mesh = Mesh { vertices :: Vector Vert
                 , indices :: Vector Ind
                 } 

loadMeshObj :: FilePath -> IO Mesh
loadMeshObj name = do
  inp <- BL.readFile name
  case parse parseOBJ inp of
    Fail _ stack err -> fail $ "Error while parsing: " ++ show stack ++ ": " ++ err
    Done _ r -> do
      let WFModel{..} = extractModel r
          indlist = wfIndices
--TODO: check different length
          vertlist = zipWith V2 wfVertices wfNormals
          --vertlist = zip wfVertices wfNormals
      return Mesh { vertices = VS.fromList vertlist, indices = VS.fromList indlist }

data MeshBuffer = MeshBuffer { mvao :: VAO
                             , mbuffer :: Buffer
                             , indOffset :: Int
                             , indNumber :: Int
                             }

initMesh :: Mesh -> IO MeshBuffer
initMesh mesh = do
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
                                          ,components = 3
                                          , stride = sizeOf (undefined :: Vert)
                                          , attributeIndex = 1
                                          , sourceType = SFloat
                                          } vao

  uploadVector (indices mesh) (sizeOfV (vertices mesh)) buff
  return MeshBuffer { mvao = vao, mbuffer = buff, indOffset = (sizeOfV (vertices mesh)), indNumber = (3*(VS.length (indices mesh))) }

drawMesh :: MeshBuffer -> Pipeline -> (M44 Float) -> DrawT IO ()
drawMesh mesh pl mvM = do
        mvMloc <- getUniformLocation "modelViewMat" pl
        setUniform mvM mvMloc pl
        drawR drawCommand { primitiveType = Triangles
                          , primitivesVAO = (mvao mesh)
                          , numIndices = (indNumber mesh)
                          --, sourceData = Primitives 0
                          , sourceData = PrimitivesWithIndices (mbuffer mesh) (indOffset mesh) IWord16
                          }
