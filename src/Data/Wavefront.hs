module Data.Wavefront
       ( WFValue(..)
       , wavefrontOBJ
       , WFModel(..)
       , extractModel
       ) where

import Control.Applicative
import Control.Monad
import GHC.Generics (Generic)
import Data.ByteString (ByteString)
import Data.Scientific (Scientific)
import Data.Default
import Linear.V3 (V3(..))
import Data.Attoparsec.ByteString.Char8

type F3 = V3 Scientific
type I3 = V3 Integer

data WFValue = WFVertex F3
             | WFNormal F3
             | WFFace I3
             deriving (Show, Eq)

skipLineComment :: Parser ()
skipLineComment = void $ string "//" *> skipWhile (/= '\n') *> char '\n'

skipSeparators :: Parser ()
skipSeparators = void $ many $ skipLineComment <|> skipSpace

token :: Parser a -> Parser a
token parser = parser <* skipSeparators

symbol :: ByteString -> Parser ByteString
symbol t = token (string t <?> "symbol")

scientific' :: Parser Scientific
scientific' = token scientific

decimal' :: Integral a => Parser a
decimal' = token decimal

pointF3 :: Parser F3
pointF3 = V3 <$> scientific' <*> scientific' <*> scientific' <?> "3-point"

pointI3 :: Parser I3
pointI3 = V3 <$> decimal' <*> decimal' <*> decimal' <?> "3-index vector"

wavefrontOBJ :: Parser [WFValue]
wavefrontOBJ = skipSeparators *> many line <* endOfInput
  where line' =     WFNormal <$> (symbol "vn" *> pointF3 <?> "normal")
                <|> WFVertex <$> (symbol "v" *> pointF3 <?> "vertex")
                <|> WFFace <$> (symbol "f" *> pointI3 <?> "face")
        line = line' <* skipSeparators

data WFModel = WFModel { wfNormals :: [F3]
                       , wfVertices :: [F3]
                       , wfIndices :: [I3]
                       }
             deriving (Show, Eq, Generic)

instance Default WFModel where

extractModel :: [WFValue] -> WFModel
extractModel [] = def
extractModel (h:t) = case h of
  WFVertex a -> m { wfVertices = a : wfVertices }
  WFNormal a -> m { wfNormals = a : wfNormals }
  WFFace a -> m { wfIndices = a : wfIndices }
  where m@(WFModel {..}) = extractModel t
