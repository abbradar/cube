module Game.Characters
  (
    PlayerState(..)
  , PlayerData(..)
  , MonsterData(..)
  ) where

import Data.ByteString
import Data.Word

import Linear
import Engine.Types

import Control.Lens


data PlayerState = Idle | Running | Attack Word32 deriving (Eq,Ord,Show)


data PlayerData = PlayerData { _pPosition :: F3
                             , _pAngle :: Float
                             , _pState :: PlayerState
                             , _pObjectName :: ByteString
                             } deriving (Eq, Ord, Show)
$(makeLenses ''PlayerData)
                  
data MonsterData = MonsterData { _mPosition :: F3
                               , _mAngle :: Float
                               , _mState :: PlayerState
                               , _mObjectName :: ByteString
                               } deriving (Eq, Ord, Show)
$(makeLenses ''MonsterData)
