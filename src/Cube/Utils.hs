-- | Common functions.

module Cube.Utils where

import Control.Applicative

sumMaybeWith :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
sumMaybeWith f (Just a) (Just b) = Just (f a b)
sumMaybeWith _ ma mb = ma <|> mb
