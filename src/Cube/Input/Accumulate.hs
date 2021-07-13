-- | Accumulate input between ticks.

{-# LANGUAGE StrictData #-}

module Cube.Input.Accumulate
  ( accumulateInput
  ) where

import Control.Applicative
import Control.Monad.Fix
import Reflex

import Reflex.Combinators
import Cube.Utils
import Cube.Time

data AccumulatedInput = AccumulatedInput { inputPressedSince :: Maybe Timestamp
                                         , inputPressedTime :: TimeInterval
                                         }
                      deriving (Show, Eq)

accumulateInput :: forall t m. (Reflex t, MonadFix m, MonadHold t m) => Event t TimeStep -> Event t (TimeStep,  Bool) -> m (Event t TimeInterval)
accumulateInput tickEvent inputEvent = foldPullEvent processEvent noInput $ mergeWith doMerge [tickEvent', inputEvent']

  where noInput = AccumulatedInput { inputPressedSince = Nothing
                                   , inputPressedTime = 0
                                   }

        tickEvent' = fmap (\tick -> (tick, Nothing, True)) tickEvent
        inputEvent' = fmap (\(tick, pressed) -> (tick, Just pressed, False)) inputEvent
        doMerge = (\(_tick, pressedA, tickedA) (tick, pressedB, tickedB) -> (tick, pressedA <|> pressedB, tickedA || tickedB))

        processEvent :: (TimeStep, Maybe Bool, Bool) -> AccumulatedInput -> (AccumulatedInput, Maybe TimeInterval)
        processEvent (TimeStep {..}, mevent, isTicked) accumulator = (nextAccumulator, integratedEvent)
          where passedTicks someAcc =
                  case inputPressedSince someAcc of
                    Nothing -> 0
                    Just since -> currentTime - since

                accumulator' =
                  case mevent of
                    Just isPressed ->
                      if isPressed then
                        accumulator { inputPressedSince = sumMaybeWith later (inputPressedSince accumulator) (Just currentTime) }
                      else
                        accumulator { inputPressedSince = Nothing, inputPressedTime = inputPressedTime accumulator + passedTicks accumulator }
                    _ -> accumulator
                -- Each tick we start calculating anew.
                nextAccumulator
                  | not isTicked = accumulator'
                  | otherwise = AccumulatedInput { inputPressedSince = fmap (const currentTime) (inputPressedSince accumulator'), inputPressedTime = 0 }

                totalTime = inputPressedTime accumulator' + passedTicks accumulator'
                integratedEvent
                  | isTicked && totalTime > 0 = Just totalTime
                  | otherwise = Nothing
