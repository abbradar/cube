-- | Helpful functions for converting constructor tags and record fields.

module Data.Aeson.Utils where

import Data.Char
import Data.List

removePrefix :: String -> String -> String
removePrefix prefix (stripPrefix prefix -> Just (c : cs)) = toLower c : cs
removePrefix _ str = str

splitNumbers :: String -> String
splitNumbers "" = ""
splitNumbers (sc : scs)
  | isDigit sc = sc : nextDigit scs
  | otherwise = toUpper sc : nextChar scs
  where nextDigit "" = ""
        nextDigit (c : cs)
          | not (isDigit c) = '_' : toUpper c : nextChar cs
          | otherwise = c : nextDigit cs

        nextChar "" = ""
        nextChar (c : cs)
          | isDigit c = '_' : c : nextDigit cs
          | otherwise = toUpper c : nextChar cs
