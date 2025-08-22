module Cube.ECS
  ( Archetype
  , Component(..)
  , Entity(..)
  , Column(..)
  , World
  , newWorld
  , addEntity
  , removeEntity
  , nextEntity
  , updateComponentByType
  , getComponentByType
  )
  where

import Control.Monad.State
import Control.Monad
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.ByteString as B
import Data.Maybe
import Linear

import Cube.Graphics.Model
import Cube.Graphics.Types
import Cube.Graphics.Assets
import Cube.Types

-- just an id for all entities in the game
  deriving (Eq, Show, Num)


data Component = CTransform TRSF | CModel FilePath | CCamera CameraF | CPlayer Int
  deriving (Show)


-- columns contain data of a particular component.
data Column = Column { columnData :: V.Vector Component
                     }
  deriving (Show)

-- rows specify which entities have this archetype
-- colomns specify the data of all the components indexed by rows,
-- that is if aRows[i] = ent, then
-- (columnData (aColumns comp)) V.!! i contains
-- the data of the component comp of the entity ent
data Archetype = Archetype { aColumns :: HM.HashMap Int Column
                           , aRows :: V.Vector Entity
                           , aType :: HS.HashSet Int
                           }
  deriving (Show)

emptyAtype :: Archetype
emptyAtype = Archetype{ aColumns = HM.empty, aRows = V.empty, aType = HS.empty }

newArchetype :: Entity -> HM.HashMap Int Column -> Archetype
newArchetype ent mp = Archetype { aColumns = mp, aRows = V.singleton ent, aType = HM.keysSet mp }

-- unsafely adds an entity to the archetype appending the corresponding columns
addToAtype :: Entity -> HM.HashMap Int Column -> Archetype -> Archetype
addToAtype ent mp atype@Archetype{..} = atype { aRows = V.snoc aRows ent, aColumns = HM.mapWithKey update mp }
  where
    update key value = Column{ columnData = (V.++) (columnData $ mp HM.! key) (columnData value)}


removeFromAtype :: Entity -> Archetype -> Archetype
removeFromAtype ent atype@Archetype{..} = let idx = V.findIndex (== ent) aRows in case idx of
  Nothing -> atype
  Just idx' -> if atypeIsSingleton atype then emptyAtype
               else atype { aColumns = newColumns, aRows = newRows }
               where
                 newRows = remove aRows idx'
                 remove vect i = V.slice 0 i vect V.++ V.slice (i+1) (V.length vect - i - 1) vect
                 newColumns = HM.map remove' aColumns
                 remove' Column{..} = Column{ columnData = remove columnData idx' }

atypeIsSingleton :: Archetype -> Bool
atypeIsSingleton Archetype{..} = V.length aRows == 1

data World = World { worldAtypes :: V.Vector Archetype
                   -- entities with corresponding archetype keys
                   , worldEntities :: V.Vector (Entity, HS.HashSet Int)
                   -- removed entities that can be reused
                   , worldFreeEntities :: V.Vector Entity
                   , worldId :: Int
                   }
  deriving (Show)

newWorld :: Int -> World
newWorld n = World { worldAtypes = V.empty, worldEntities = V.empty, worldFreeEntities = V.singleton 0, worldId = n }

nextEntity :: World -> Entity
nextEntity World{..} = V.last worldFreeEntities

addEntity :: MonadCube m => HM.HashMap Int Column -> World -> SceneT m World
addEntity mp wrld@World{..} = do
  SceneInfo{..} <- get
  unless (isNothing $ HM.lookup 1 mp) $ modify updtScene
  return $ wrld{ worldAtypes = newAtypes, worldEntities = V.snoc worldEntities (ent, HM.keysSet mp), worldFreeEntities = wFE }
  where
    updtScene = undefined
    ent = V.last worldFreeEntities
    wFE = if V.length worldFreeEntities == 1 then V.singleton (ent+1) else V.init worldFreeEntities
    newAtypes = case entAtype of
      Nothing -> V.snoc worldAtypes $ newArchetype ent mp
      Just atype -> worldAtypes V.// [(atype, addToAtype ent mp (worldAtypes V.! atype))]
    entAtype = V.findIndex (\x -> aType x == HM.keysSet mp) worldAtypes

removeElement :: V.Vector a -> Int -> V.Vector a
removeElement v i = V.slice 0 i v V.++ V.slice (i+1) (V.length v - i - 1) v

removeEntity :: Entity -> World -> World
removeEntity ent wrld@World{..} =
  case V.findIndex (\x -> fst x == ent) worldEntities of
    Nothing -> wrld
    Just entIndex -> wrld{ worldEntities = newEntities, worldFreeEntities = V.snoc worldFreeEntities (fst $ worldEntities V.! entIndex), worldAtypes = newAtypes }
      where
        entAtype = fromJust $ V.findIndex (\x -> aType x == snd (worldEntities V.! entIndex)) worldAtypes
        newEntities = removeElement worldEntities entIndex
        newAtypes = if atypeIsSingleton (worldAtypes V.! entAtype) then removeElement worldAtypes entAtype
                                                                   else worldAtypes V.// [(entAtype, removeFromAtype ent (worldAtypes V.! entAtype))]

-- updates a particular component specified by comp for all entities containing asign components
updateComponentByType :: HS.HashSet Int -> Int -> (Component -> Component) -> World -> World
updateComponentByType asign comp f wrld@World{..} = wrld{ worldAtypes = fmap runAtype worldAtypes}
  where
    runAtype :: Archetype -> Archetype
    runAtype at = if not $ HS.isSubsetOf asign $ aType at then at else at{ aColumns = HM.adjust (\Column{..} -> Column{ columnData = fmap f columnData }) comp (aColumns at) }


getComponentByType :: HS.HashSet Int -> Int -> World -> V.Vector (V.Vector Component)
getComponentByType asign comp wrld@World{..} = fmap (\x -> columnData $ (aColumns x) HM.! comp) $ V.filter (\x -> HS.isSubsetOf asign $ aType x) worldAtypes
