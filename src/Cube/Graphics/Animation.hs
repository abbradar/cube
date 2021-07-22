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
  , nodeAnimationMorph
  ) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Generic as VG
import qualified Data.HashMap.Strict as HM
import Linear

import Data.Int
import Data.Vector.Functor
import qualified Data.GlTF.Types as TF
import Cube.Graphics.Model
import Cube.Graphics.Types
import Cube.Graphics.TRS
import Cube.Time

class UpdateAnimation f where
  updateAnimation :: (forall container component. (Num (component Float), VG.Vector container (component Float), UnboxFunctor component) => LoadedSampler container component meta1 -> meta2 container component) -> f meta1 -> f meta2

instance (Num (component Float), VG.Vector container (component Float), UnboxFunctor component) => UpdateAnimation (LoadedSampler container component) where
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
  updateAnimation f anim = anim { lanimNodes = HM.map (updateAnimation f) $ lanimNodes anim }
  {-# INLINE updateAnimation #-}

data AnimationState = AnimationState { astateAnimation :: LoadedAnimation SamplerState
                                     , astateSince :: Timestamp
                                     , astateFinished :: Bool
                                     , astateLoop :: Bool
                                     }

data SamplerState container component = SamplerState { sstateNow :: component Float
                                                     , sstateIndex :: Int
                                                     }

advanceSamplerState :: (Num (component Float), VG.Vector container (component Float), UnboxFunctor component) => Float -> LoadedSampler container component SamplerState -> SamplerState container component
advanceSamplerState currentTime (LoadedSampler {..})
  | currentTime >= lsampEnd = finalState
  | currentTime <= lsampBeginning = initialState
  | otherwise = SamplerState { sstateNow = nowValue
                             , sstateIndex = newIndex
                             }

  where finalState = SamplerState { sstateNow = VG.last lsampOutputs
                                  , sstateIndex = VS.length lsampInputs
                                  }
        initialState = SamplerState { sstateNow = VG.head lsampOutputs
                                    , sstateIndex = 0
                                    }

        (endTime, newIndex) = findNewIndex $ sstateIndex lsampMeta
        startTime = lsampInputs VG.! newIndex

        startLinearOutput = lsampOutputs VG.! newIndex
        endLinearOutput = lsampOutputs VG.! (newIndex + 1)
        linearK = (currentTime - startTime) / (endTime - startTime)
        nowValue =
          case lsampInterpolation of
            TF.ASIStep -> startLinearOutput
            TF.ASILinear -> fmapUnbox (* linearK) startLinearOutput + fmapUnbox (* (1 - linearK)) endLinearOutput
            TF.ASICubicSpline -> error "FIXME: Cubic splines are not supported now"

        findNewIndex i
          | currentTime >= currEndTime = findNewIndex (i + 1)
          | otherwise = (currEndTime, i)
          where currEndTime = lsampInputs VS.! (i + 1)
{-# INLINE advanceSamplerState #-}

advanceAnimation :: Timestamp -> AnimationState -> AnimationState
advanceAnimation ts state@(AnimationState {..})
  | astateFinished = state
  | newNow >= lanimEnd astateAnimation =
    if astateLoop then
      state { astateAnimation = updateAnimation loopState astateAnimation
            , astateSince = loopedNewSince
            }
    else
      state { astateAnimation = updateAnimation (advanceSamplerState newNow) astateAnimation
            , astateFinished = True
            }
  | otherwise = state { astateAnimation = updateAnimation (advanceSamplerState newNow) astateAnimation }

  where newNow = intervalSeconds (ts - astateSince)

        loopedNow = newNow - lanimEnd astateAnimation * fromIntegral (floor (newNow / lanimEnd astateAnimation) :: Int32)
        loopedNewSince = ts - secondsInterval loopedNow

        loopState :: forall component container. (Num (component Float), VG.Vector container (component Float), UnboxFunctor component) => LoadedSampler container component SamplerState -> SamplerState container component
        loopState sampler = advanceSamplerState loopedNow $ sampler { lsampMeta = (lsampMeta sampler) { sstateIndex = 0 } }

data AnimationOptions = AnimationOptions { aoptsLoop :: Bool }
                      deriving (Show, Eq)

defaultAnimationOptions :: AnimationOptions
defaultAnimationOptions = AnimationOptions { aoptsLoop = False }

startAnimation :: Timestamp -> LoadedAnimation EmptySamplerState -> AnimationOptions -> AnimationState
startAnimation astateSince animation (AnimationOptions {..}) =
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

groupAnimationMorph :: LoadedSamplerGroup SamplerState -> TRSF
groupAnimationMorph (LoadedSamplerGroup {..}) =
  TRS { trsTranslation = maybe 0 (sstateNow . lsampMeta) samplerTranslation
      , trsRotation = maybe (Quaternion 1 0) (sstateNow . lsampMeta) samplerRotation
      , trsScale = maybe 1 (sstateNow . lsampMeta) samplerScale
      }

nodeAnimationMorph :: TF.NodeIndex -> LoadedAnimation SamplerState -> Maybe TRSF
nodeAnimationMorph nodeIndex = fmap groupAnimationMorph . HM.lookup nodeIndex . lanimNodes
