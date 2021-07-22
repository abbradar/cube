-- | Generic coercion for containers.

module Data.Coerce.Functor where

import Data.Coerce

class Coercible1 f where
  coerce1 :: Coercible a b => f a -> f b
  default coerce1 :: (Coercible (f a) (f b)) => f a -> f b
  coerce1 = coerce

instance Coercible1 []

instance Coercible1 Maybe
