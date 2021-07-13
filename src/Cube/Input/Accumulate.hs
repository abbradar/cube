-- | Accumulate input between ticks.

{-# LANGUAGE StrictData #-}

module Cube.Input.Accumulate
  ( InputEvent(..)
  , AccumulatedInput
  , accumulateInput
  , sumInputTime
  ) where

import Control.Applicative
import Witherable
import Control.Monad.Fix
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Reflex
import Debug.Trace
import qualified SDL

import Cube.Loop.Stable

data InputEvent = InputEvent { inputPressedNow :: Bool
                             , inputElapsedTime :: TimeInterval
                             }
                deriving (Show, Eq)

type AccumulatedInput k = Map k [InputEvent]

-- The first field is time since the key has been pressed now.
type InputAccumulator k = Map k (Maybe SDL.Timestamp, [InputEvent])

sumMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
sumMaybeWith f (Just a) (Just b) = Just (f a b)
sumMaybeWith _ ma mb = ma <|> mb

accumulateInput :: forall k t m. (Show k, Ord k, Reflex t, MonadFix m, MonadHold t m) => Event t CubeTickInfo -> Event t (CubeTickInfo, (k, Bool)) -> m (Event t (AccumulatedInput k))
accumulateInput tickEvent inputEvent = do
  integratedDyn <- foldDyn processEvent (M.empty, Nothing) $ mergeWith doMerge [tickEvent', inputEvent']
  return $ mapMaybe snd (updated integratedDyn)

  where tickEvent' = fmap (\tick -> (tick, Nothing, True)) tickEvent
        inputEvent' = fmap (\(tick, payload) -> (tick, Just payload, False)) inputEvent
        doMerge = (\(_tick, payloadA, tickedA) (tick, payloadB, tickedB) -> (tick, payloadA <|> payloadB, tickedA || tickedB))

        processEvent :: (CubeTickInfo, Maybe (k, Bool), Bool) -> (InputAccumulator k, Maybe (AccumulatedInput k)) -> (InputAccumulator k, Maybe (AccumulatedInput k))
        processEvent (CubeTickInfo {..}, mevent, isTicked) (accumulator, _) = (nextAccumulator, integratedEvent)
          where accumulator' =
                  case mevent of
                    Just (inputKey, isPressed) ->
                      if isPressed then
                        M.insertWith (\(pressedA, _newEvents) (pressedB, events) -> (sumMaybeWith max pressedA pressedB, events)) inputKey (Just currentTime, []) accumulator
                      else
                        let process st@(pressed, events) =
                              case pressed of
                                Nothing -> st
                                Just since -> (Nothing, InputEvent { inputPressedNow = False, inputElapsedTime = currentTime - since } : events)
                        in M.adjust process inputKey accumulator
                    _ -> accumulator
                -- Each tick we drop keys that are no longer pressed and start calculating anew.
                nextAccumulator
                  | not isTicked = accumulator'
                  | otherwise =
                    let process (pressed, _events) =
                          case pressed of
                            Nothing -> Nothing
                            Just _since -> Just (Just currentTime, [])
                    in mapMaybe process accumulator'

                integratedKeys =
                  let process (pressed, events) =
                        let events' =
                              case pressed of
                                Nothing -> events
                                Just since -> InputEvent { inputPressedNow = True, inputElapsedTime = currentTime - since } : events
                        in reverse events'
                  in fmap process accumulator'
                integratedEvent
                  | isTicked = Just $ Debug.Trace.trace ("integrated " ++ show integratedKeys) integratedKeys
                  | otherwise = Nothing

sumInputTime :: [InputEvent] -> TicksElapsed
sumInputTime = foldr (\event time -> time + inputElapsedTime event) 0
