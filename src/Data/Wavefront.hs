module Data.Wavefront
       ( WFValue(..)
       , wavefrontOBJ
       , WFModel(..)
       , extractModel
       ) where

import Control.Applicative
import Data.Scientific (Scientific)
import Data.Default
import Linear.V3 (V3(..))
import Text.Parser.Combinators
import Text.Parser.Token

import Text.Parser.Comments

type F3 = V3 Scientific
type I3 = V3 Integer

data WFValue = WFVertex F3
             | WFNormal F3
             | WFFace I3
             deriving (Show, Eq)

type OBJParser a = forall m. (Monad m, TokenParsing m) => LineCommentT "#" m a

number :: OBJParser Scientific
number = either fromIntegral id <$> integerOrScientific

pointF3 :: OBJParser F3
pointF3 = V3 <$> number <*> number <*> number <?> "3-point"

pointI3 :: OBJParser I3
pointI3 = V3 <$> natural <*> natural <*> natural <?> "3-index vector"

wavefrontOBJ :: (Monad m, TokenParsing m) => m [WFValue]
wavefrontOBJ = runLineCommentT $ optional someSpace *> many line <* eof
  where line =     WFNormal <$> (symbol "vn" *> pointF3 <?> "normal")
               <|> WFVertex <$> (symbol "v" *> pointF3 <?> "vertex")
               <|> WFFace <$> (symbol "f" *> pointI3 <?> "face")

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

extractModel :: [WFValue] -> WFModel
extractModel [] = def
extractModel (h:t) = case h of
  WFVertex a -> m { wfVertices = a : wfVertices }
  WFNormal a -> m { wfNormals = a : wfNormals }
  WFFace a -> m { wfIndices = a : wfIndices }
  where m@(WFModel {..}) = extractModel t
