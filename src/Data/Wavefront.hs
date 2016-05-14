module Data.Wavefront
       ( Wavefront(..)
       , parseOBJ
       ) where

import Control.Monad
import Control.Applicative
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import Linear.V3 (V3(..))
import Data.Attoparsec.ByteString.Char8

type F3 = V3 Float

data Wavefront = WFVertex F3
               | WFNormal F3
               deriving (Show, Eq)

float :: Parser Float
float = fromRational <$> toRational <$> double

objSpace :: Parser ()
objSpace = skipWhile (== ' ')

objNumber :: Parser Float
objNumber = objSpace *> float

point3 :: Parser F3
point3 = V3 <$> objNumber <*> objNumber <*> objNumber

skipComments :: Parser ()
skipComments = skipSpace <* optional (char '#' >> skipWhile (/= '\n') >> skipComments)

prefix :: ByteString -> Parser ()
prefix s = void $ string s *> char ' '

parseOBJ :: Parser (Vector Wavefront)
parseOBJ = V.fromList <$> many (skipComments *> line)
  where line =     WFVertex <$> (prefix "v" *> point3)
               <|> WFNormal <$> (prefix "vn" *> point3)
