module Data.Wavefront
       ( Wavefront(..)
       , parseOBJ
       , WFModel(..)
       , extractModel
       ) where

import Control.Monad
import Control.Applicative
import Data.Word (Word16)
import Data.Default
import Data.ByteString (ByteString)
import Linear.V3 (V3(..))
import Data.Attoparsec.ByteString.Char8

type F3 = V3 Float
type I3 = V3 Word16

data Wavefront = WFVertex F3
               | WFNormal F3
               | WFFace I3
               deriving (Show, Eq)

float :: Parser Float
float = fromRational <$> toRational <$> double

objSpace :: Parser ()
objSpace = skipWhile (== ' ')

objFloat :: Parser Float
objFloat = objSpace *> float

objWord :: Parser Word16
objWord = objSpace *> decimal

pointF3 :: Parser F3
pointF3 = V3 <$> objFloat <*> objFloat <*> objFloat

pointI3 :: Parser I3
pointI3 = V3 <$> objWord <*> objWord <*> objWord

skipComments :: Parser ()
skipComments = skipSpace <* optional (char '#' >> skipWhile (/= '\n') >> skipComments)

prefix :: ByteString -> Parser ()
prefix s = void $ string s *> char ' '

parseOBJ :: Parser [Wavefront]
parseOBJ = many (skipComments *> line) <* skipSpace <* endOfInput
  where line =     WFVertex <$> (prefix "v" *> pointF3)
               <|> WFNormal <$> (prefix "vn" *> pointF3)
               <|> WFFace <$> (prefix "f" *> pointI3)

data WFModel = WFModel { wfNormals :: [F3]
                       , wfVertices :: [F3]
                       , wfIndices :: [I3]
                       }
             deriving (Show, Eq)

-- FIXME: replace when data-default-class is updated on Hackage
instance Default WFModel where
  def = WFModel { wfNormals = []
                , wfVertices = []
                , wfIndices = []
                }

extractModel :: [Wavefront] -> WFModel
extractModel [] = def
extractModel (h:t) = case h of
  WFVertex a -> m { wfVertices = a : wfVertices }
  WFNormal a -> m { wfNormals = a : wfNormals }
  WFFace a -> m { wfIndices = a : wfIndices }
  where m@(WFModel {..}) = extractModel t
