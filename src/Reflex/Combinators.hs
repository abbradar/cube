-- | Extra combinators for Reflex.

module Reflex.Combinators where

import Control.Monad.Fix
import Reflex

foldPullEvent :: (Reflex t, MonadFix m, MonadHold t m) => (a -> b -> (b, Maybe c)) -> b -> Event t a -> m (Event t c)
foldPullEvent f initial event = do
  stateDyn <- foldDyn (\payload (state, _old) -> f payload state) (initial, Nothing) event
  return $ mapMaybe snd (updated stateDyn)
