-- | Advance animation.

{-# LANGUAGE StrictData #-}

module Cube.Graphics.Animation
  ( UpdateAnimation(..)
  , AnimationState(..)
  , SamplerState(..)
  , AnimationOptions(..)
  , defaultAnimationOptions
  , startAnimation
  , advanceAnimation
  , groupAnimationMorph
  ) where

import Data.Maybe
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import qualified Data.IntMap.Strict as IM
import Linear hiding (trace)

import Data.Int
import Data.Vector.Functor
import qualified Data.GlTF.Types as TF
import Cube.Graphics.Model
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Time

type UpdatableAnimation container component = (Num (component Float), VG.Vector container (component Float), UnboxFunctor component)

class UpdateAnimation f where
  updateAnimation :: (forall container component. UpdatableAnimation container component => LoadedSampler container component meta1 -> meta2 container component) -> f meta1 -> f meta2

instance UpdatableAnimation container component => UpdateAnimation (LoadedSampler container component) where
  updateAnimation f sampler = sampler { lsampMeta = f sampler }
  {-# INLINE updateAnimation #-}

instance UpdateAnimation LoadedSamplerGroup where
  updateAnimation f (LoadedSamplerGroup {..}) = LoadedSamplerGroup { samplerTranslation = fmap (updateAnimation f) samplerTranslation
                                                                   , samplerRotation = fmap (updateAnimation f) samplerRotation
                                                                   , samplerScale = fmap (updateAnimation f) samplerScale
                                                                   , samplerWeights = fmap (updateAnimation f) samplerWeights
                                                                   }
  {-# INLINE updateAnimation #-}

instance UpdateAnimation LoadedAnimation where
  updateAnimation f anim = anim { lanimNodes = IM.map (updateAnimation f) $ lanimNodes anim }
  {-# INLINE updateAnimation #-}

data AnimationState = AnimationState { astateAnimation :: LoadedAnimation SamplerState
                                     , astateSince :: Maybe Timestamp
                                     , astateFinished :: Bool
                                     , astateLoop :: Bool
                                     }

data SamplerState container component = SamplerState { sstateNow :: component Float
                                                     , sstateIndex :: Int
                                                     }

deriving instance Show (component Float) => Show (SamplerState accessor component)

advanceSamplerState :: UpdatableAnimation container component => Float -> LoadedSampler container component SamplerState -> SamplerState container component
advanceSamplerState currentTime (LoadedSampler {..})
  | currentTime >= lsampEnd = finalState
  | currentTime <= lsampBeginning = initialState
  | otherwise = currentState

  where finalState = SamplerState { sstateNow = VG.last lsampOutputs
                                  , sstateIndex = VS.length lsampInputs
                                  }
        initialState = SamplerState { sstateNow = VG.head lsampOutputs
                                    , sstateIndex = 0
                                    }
        currentState = SamplerState { sstateNow = nowValue
                                    , sstateIndex = newIndex
                                    }

        (endTime, newIndex) = findNewIndex $ sstateIndex lsampMeta
        startTime = lsampInputs VG.! newIndex

        startLinearOutput = lsampOutputs VG.! newIndex
        endLinearOutput = lsampOutputs VG.! (newIndex + 1)
        linearK = (currentTime - startTime) / (endTime - startTime)
        nowValue =
          case lsampInterpolation of
            TF.ASIStep -> startLinearOutput
            TF.ASILinear -> fmapUnbox (* linearK) endLinearOutput + fmapUnbox (* (1 - linearK)) startLinearOutput
            TF.ASICubicSpline -> error "FIXME: Cubic splines are not supported now"

        findNewIndex i
          | currentTime >= currEndTime = findNewIndex (i + 1)
          | otherwise = (currEndTime, i)
          where currEndTime = lsampInputs VS.! (i + 1)
{-# INLINE advanceSamplerState #-}

advanceAnimation :: Timestamp -> AnimationState -> AnimationState
advanceAnimation currentTime state@(AnimationState {..})
  | astateFinished = state
  -- Set `since` here, because if `since` is not set this condititon will always fire.
  | newNow <= lanimBeginning astateAnimation = state { astateSince = Just since }
  | newNow >= lanimEnd astateAnimation =
    if astateLoop then
      state { astateAnimation = updateAnimation loopState astateAnimation
            , astateSince = Just loopedNewSince
            }
    else
      state { astateAnimation = updateAnimation (advanceSamplerState newNow) astateAnimation
            , astateFinished = True
            }
  | otherwise = state { astateAnimation = updateAnimation (advanceSamplerState newNow) astateAnimation }

  where newNow = intervalSeconds (currentTime - since)
        since = fromMaybe currentTime astateSince

        loopedNow = newNow - lanimEnd astateAnimation * fromIntegral (floor (newNow / lanimEnd astateAnimation) :: Int32)
        loopedNewSince = currentTime - secondsInterval loopedNow

        loopState :: forall component container. UpdatableAnimation container component => LoadedSampler container component SamplerState -> SamplerState container component
        loopState sampler = advanceSamplerState loopedNow $ sampler { lsampMeta = (lsampMeta sampler) { sstateIndex = 0 } }

data AnimationOptions = AnimationOptions { aoptsLoop :: Bool }
                      deriving (Show, Eq)

defaultAnimationOptions :: AnimationOptions
defaultAnimationOptions = AnimationOptions { aoptsLoop = False }

startAnimation :: AnimationOptions -> Maybe Timestamp -> LoadedAnimation EmptySamplerState -> AnimationState
startAnimation (AnimationOptions {..}) astateSince animation =
  AnimationState { astateFinished = lanimEnd animation == 0
                 , astateLoop = aoptsLoop
                 , astateSince
                 , astateAnimation = updateAnimation setInitial animation
                 }
  where setInitial :: (Num (component Float), VG.Vector container (component Float), UnboxFunctor component) => LoadedSampler container component EmptySamplerState -> SamplerState container component
        setInitial sampler =
          SamplerState { sstateNow = VG.head $ lsampOutputs sampler
                       , sstateIndex = 0
                       }


--TODO do something with these quaternions

convertQuaternion :: Quaternion a -> Quaternion a
convertQuaternion (Quaternion x (V3 y z w)) = Quaternion w (V3 x y z)

groupAnimationMorph :: LoadedSamplerGroup SamplerState -> TRSF
groupAnimationMorph (LoadedSamplerGroup {..}) =
  TRS { trsTranslation = maybe 0 (sstateNow . lsampMeta) samplerTranslation
      , trsRotation = convertQuaternion $ maybe (Quaternion 0 (V3 0 0 1)) (sstateNow . lsampMeta) samplerRotation
      , trsScale = maybe 1 (sstateNow . lsampMeta) samplerScale
      }
