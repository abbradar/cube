{

module Data.Wavefront.Parser where

import Data.Word
import qualified Data.ByteString.Lazy as BL
import qualified Data.Scientific as S
import Linear.V3 (V3(..))

import Data.Wavefront.Lexer

import Debug.Trace

}

%name parseOBJInt
%tokentype { Token }
%error { parserFail }
%monad { Alex }
%lexer { nextToken } { EOF }

%token
  v { V }
  vn { VN }
  f { F }
  num { Num $$ }

%%

Begin :: { [WFValue] }
Begin : Item Begin            { $1 : $2     }
      | {- empty -}           { []          }

Item : Vertex                 { WFVertex $1 }
     | Normal                 { WFNormal $1 }
     | Face                   { WFFace $1   }

Vertex : v Float Float Float  { V3 $2 $3 $4 }
Normal : vn Float Float Float { V3 $2 $3 $4 }
Face : f Int Int Int          { V3 $2 $3 $4 }

Int : num                     {% maybe (fail "invalid integer") return $ S.toBoundedInteger $1 }
Float : num                   { S.toRealFloat $1 }

{

type F3 = V3 Float
type I3 = V3 Word16

data WFValue = WFVertex F3
             | WFNormal F3
             | WFFace I3
             deriving (Show, Eq)

nextToken :: (Token -> Alex a) -> Alex a
nextToken next = alexMonadScan >>= \x -> traceShow x $ next x

parserFail :: Token -> Alex a
parserFail t = fail $ "Unexpected token: " ++ show t

parseOBJ :: BL.ByteString -> Either String [WFValue]
parseOBJ s = runAlex s parseOBJInt

}
