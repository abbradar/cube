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
import qualified SDL

import Reflex.Combinators
import Cube.Utils
import Cube.Time
import Cube.Loop.Stable

data InputEvent = InputEvent { inputPressedNow :: Bool
                             , inputElapsedTime :: TimeInterval
                             }
                deriving (Show, Eq)

type AccumulatedInput k = Map k [InputEvent]

-- The first field is time since the key has been pressed now.
type InputAccumulator k = Map k (Maybe SDL.Timestamp, [InputEvent])

accumulateInput :: forall k t m. (Ord k, Reflex t, MonadFix m, MonadHold t m) => Event t CubeTickInfo -> Event t (CubeTickInfo, (k, Bool)) -> m (Event t (AccumulatedInput k))
accumulateInput tickEvent inputEvent = foldPullEvent processEvent M.empty $ mergeWith doMerge [tickEvent', inputEvent']

  where tickEvent' = fmap (\tick -> (tick, Nothing, True)) tickEvent
        inputEvent' = fmap (\(tick, payload) -> (tick, Just payload, False)) inputEvent
        doMerge = (\(_tick, payloadA, tickedA) (tick, payloadB, tickedB) -> (tick, payloadA <|> payloadB, tickedA || tickedB))

        processEvent :: (CubeTickInfo, Maybe (k, Bool), Bool) -> InputAccumulator k -> (InputAccumulator k, Maybe (AccumulatedInput k))
        processEvent (CubeTickInfo {..}, mevent, isTicked) accumulator = (nextAccumulator, integratedEvent)
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
                  | isTicked = Just integratedKeys
                  | otherwise = Nothing

sumInputTime :: [InputEvent] -> TicksElapsed
sumInputTime = foldr (\event time -> time + inputElapsedTime event) 0
