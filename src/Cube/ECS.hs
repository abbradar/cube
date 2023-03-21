module Cube.ECS
  ( Archetype
  , Entity(..)
  , Column(..)
  , World(..)
  , addEntity
  , removeEntity
  )
  where

import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import Data.Maybe (isNothing)

-- contains id and size of each component type
newtype Entity = Entity Int
  deriving (Eq, Show)

--newtype Component = Component Int
--  deriving (Eq, Show)

-- columns contain data of a particular component. The data can be unsafely typecasted from the bytestring
data Column = Column { columnData :: B.ByteString
                     , columnShift :: Int
                     }


-- rows specify which entities have this archetype
-- colomns specify the data of all the components indexed by rows, that is if aRows[i] = ent, then (columnData (aColumns comp))[i*(columnType (aColumns comp))] contains
-- the data of the component comp of the entity ent
data Archetype = Archetype { aColumns :: HM.HashMap Int Column
                           , aRows :: V.Vector Entity
                           , aType :: V.Vector Int
                           }

emptyAtype :: Archetype
emptyAtype = Archetype{ aColumns = HM.empty, aRows = V.empty, aType = V.empty }

newArchetype :: Entity -> HM.HashMap Int Column -> Archetype
newArchetype ent mp = Archetype { aColumns = mp, aRows = V.singleton ent, aType = V.fromList $ map fst $ HM.toList mp }

-- unsafely adds an entity to the archetype appending the corresponding columns
addToAtype :: Entity -> HM.HashMap Int Column -> Archetype -> Archetype
addToAtype ent mp atype@Archetype{..} = atype { aRows = V.snoc aRows ent, aColumns = HM.mapWithKey update mp }
  where
    update key value = Column{ columnData = B.append (columnData $ mp HM.! key) (columnData value), columnShift = columnShift value}


removeFromAtype :: Entity -> Archetype -> Archetype
removeFromAtype ent atype@Archetype{..} = let idx = V.findIndex (\x -> x == ent) aRows in case idx of
  Nothing -> atype
  Just idx' -> if atypeIsSingleton atype then emptyAtype
               else atype { aColumns = newColumns, aRows = newRows }
               where
                 newRows = remove aRows idx'
                 remove vect i = V.slice 0 i vect V.++ V.slice (i+1) (V.length vect - i - 1) vect
                 newColumns = HM.map remove' aColumns
                 remove' Column{..} = Column{ columnData = B.append (B.take (idx'*columnShift) columnData) (B.drop ((idx'+1)*columnShift) columnData), columnShift }

atypeIsSingleton :: Archetype -> Bool
atypeIsSingleton Archetype{..} = V.length aRows == 1

data World = World { worldAtypes :: V.Vector Archetype
                   , worldEntities :: V.Vector Entity
                   , worldId :: Int
                   }

addEntity :: Entity -> HM.HashMap Int Column -> World -> World
addEntity ent mp wrld = undefined

removeEntity :: Entity -> World -> World
removeEntity ent wrld = undefined
