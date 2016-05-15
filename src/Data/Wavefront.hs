module Data.Wavefront
       ( Wavefront(..)
       , F3
       , I3
       , parseOBJ
       , extractModel
       , WFModel(..)
       ) where

import Control.Monad
import Control.Applicative
import Data.Default
import Data.Word (Word16)
import Data.ByteString (ByteString)
import Linear.V3 (V3(..))
import Data.Attoparsec.ByteString.Char8
import GHC.Generics (Generic)

type F3 = V3 Float
type I3 = V3 Word16

data Wavefront = WFVertex F3
               | WFNormal F3
               | WFFace I3
               deriving (Show, Eq, Generic)

data WFModel = WFModel { wfNormals :: [F3], wfVertices :: [F3], wfIndices :: [I3] }
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

float :: Parser Float
float = fromRational <$> toRational <$> double

objSpace :: Parser ()
objSpace = skipWhile (== ' ')

objNumber :: Parser Float
objNumber = objSpace *> float

objiNumber :: Parser Word16
objiNumber = objSpace *> decimal

point3 :: Parser F3
point3 = V3 <$> objNumber <*> objNumber <*> objNumber

ipoint3 :: Parser I3
ipoint3 = V3 <$> objiNumber <*> objiNumber <*> objiNumber

skipComments :: Parser ()
skipComments = skipSpace <* optional (char '#' >> skipWhile (/= '\n') >> skipComments)

prefix :: ByteString -> Parser ()
prefix s = void $ string s *> char ' '

parseOBJ :: Parser ([Wavefront])
parseOBJ = many (skipComments *> line) <* skipSpace <* endOfInput
  where line =     WFVertex <$> (prefix "v" *> point3)
               <|> WFNormal <$> (prefix "vn" *> point3)
               <|> WFFace <$> (prefix "f" *> ipoint3)


