{

module Data.Wavefront.Lexer where

import Data.Scientific (Scientific)
import qualified Data.ByteString.Lazy.Char8 as BL

}

%wrapper "monad-bytestring"

$digit = [0-9]
@num = \-?$digit+(\.$digit*)?([eE][\+\-]?$digit+)?

tokens :-
  $white+  ;
  "#".*    ;
  "v"      { cval V }
  "vn"     { cval VN }
  "f"      { cval F }
  @num   { rval Num }

{

data Token = Num Scientific
           | V | VN | F
           | EOF
           deriving (Show, Eq)

cval :: Token -> AlexAction Token
cval t = token $ \_ _ -> t

rval :: Read a => (a -> Token) -> AlexAction Token
rval f = token $ \(_, _, str, _) len -> f (read $ take (fromIntegral len) $ BL.unpack $ str)

alexEOF :: Alex Token
alexEOF = return EOF

}
