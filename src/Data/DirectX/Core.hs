module Data.DirectX.Core
  ( ValueData(..)
  , Value(..)
  , Values
  , ParserState(..)
  , XParser
  , TName(..)
  , MName(..)
  , ValueType(..)
  , Dimension(..)
  , Type(..)
  , Restriction(..)
  , TemplateData(..)
  , DName(..)
  , Data(..)

  , skipSeparators
  , separators
  , name
  , guid
  , ciSymbol
  , symbol
  , ciSymbolSep
  , symbolSep
  , symChar
  , stringLiteral
  , decimalLiteral
  , signedDecLiteral
  , doubleLiteral

  , sepBy
  , sepBy1
  ) where

import Prelude hiding (takeWhile)
import Data.Int
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.String as DS
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Attoparsec.ByteString.Char8

data ValueData t = DV t
                 | DA [t]
                 deriving (Show, Eq)

data Value = VWord (ValueData Word16)
           | VDWord (ValueData Word32)
           | VFloat (ValueData Float)
           | VDouble (ValueData Double)
           | VChar (ValueData Int8)
           | VByte (ValueData Word8)
           | VString (ValueData ByteString)
           | VCustom (ValueData Values)
           deriving (Show, Eq)

type Values = Map MName Value

data ParserState = ParserState { dxTemplates :: Map TName (TemplateData, XParser Values)
                               , dxTemplateGuids :: Set UUID
                               , dxObjects :: Map DName Data
                               , dxGuidObjects :: Map UUID Data
                               }

type XParser = StateT ParserState Parser

-- Type name
newtype TName = TName ByteString
              deriving (Show, Eq, Ord, DS.IsString)

-- Member name
newtype MName = MName ByteString
              deriving (Show, Eq, Ord, DS.IsString)

data ValueType = Word
               | DWord
               | Float
               | Double
               | Char
               | UChar
               | Byte
               | String
               | Custom TName
               deriving (Show, Eq)

data Dimension = DConst Int
               | DRef MName
               deriving (Show, Eq)

data Type = Value ValueType | Array ValueType [Dimension]
          deriving (Show, Eq)

data Restriction = Closed
                 | Opened
                 | Restricted (Set TName)
                 deriving (Show, Eq)

data TemplateData = TemplateData { typeGuid :: UUID
                                 , typeMembers :: [(Type, MName)]
                                 , typeRestriction :: Restriction
                                 }
                  deriving (Show, Eq)

-- Data name
newtype DName = DName {fromDName :: ByteString}
              deriving (Show, Eq, Ord, DS.IsString)

data Data = Data { dataTemplate :: TName
                 , dataName :: Maybe DName
                 , dataGuid :: Maybe UUID
                 , dataValues :: Values
                 , dataChildren :: [Data]
                 }
          deriving (Show, Eq)

skipLineComment :: Parser ()
skipLineComment = string "//" *> skipWhile (/= '\n')

skipMultiComment :: Parser ()
skipMultiComment = string "/*" *> chunk
  where chunk = skipWhile (/= '*') *> (void (string "*/") <|> (char '*' *> chunk))

skipSeparators :: XParser ()
skipSeparators = lift $ skipSpace *> skipMany ((skipLineComment <|> skipMultiComment) <* skipSpace)

separators :: XParser ()
separators = lift (skipLineComment <|> skipMultiComment <|> void space) *> skipSeparators

token :: String -> Parser a -> XParser a
token help parser = lift (parser <?> help) <* skipSeparators

name :: XParser ByteString
name = token "name" $ do
  h <- satisfy (inClass firstSet)
  t <- takeWhile (inClass nextSet)
  return $ B.cons h t
  
  where firstSet = ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
        nextSet = firstSet ++ ['0'..'9'] ++ ['-']

guid :: XParser UUID
guid = lift (char '<' *> skipWhile (== ' ')) *> token "guid" internal <* lift (skipWhile (== ' ')) <* symChar '>'
  where internal = fromJust <$> U.fromString <$> mconcat <$> sequence
                   [ alnums 8, delim
                   , alnums 4, delim
                   , alnums 4, delim
                   , alnums 4, delim
                   , alnums 12
                   ]
        alnums :: Int -> Parser String
        alnums n = B.unpack <$> scan 0 (\i c -> if i == n then Nothing else (if inAlnumClass c then Just (i + 1) else Nothing))
        inAlnumClass = inClass (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
        delim = "-" <$ char '-'

ciSymbol :: ByteString -> XParser ByteString
ciSymbol t = token ("\"" ++ B.unpack t ++ "\"") $ stringCI t

symbol :: ByteString -> XParser ByteString
symbol t = token ("\"" ++ B.unpack t ++ "\"") $ string t

ciSymbolSep :: ByteString -> XParser ByteString
ciSymbolSep t = lift (stringCI t <?> ("\"" ++ B.unpack t ++ "\"")) <* separators

symbolSep :: ByteString -> XParser ByteString
symbolSep t = lift (string t <?> ("\"" ++ B.unpack t ++ "\"")) <* separators

symChar :: Char -> XParser Char
symChar t = token (show t) $ char t

stringLiteral :: XParser ByteString
stringLiteral = token "string literal" literal
  where literal = char '"' *> (mconcat <$> many insides) <* char '"'
        insides = takeWhile1 (notInClass "\"\\") <|> (char '\\' *> escapedChar)
        escapedChar =     "\a" <$ char 'a'
                      <|> "\b" <$ char 'b'
                      <|> "\f" <$ char 'f'
                      <|> "\n" <$ char 'n'
                      <|> "\r" <$ char 'r'
                      <|> "\t" <$ char 't'
                      <|> "\v" <$ char 'v'
                      <|> "\\" <$ char '\\'
                      <|> "'"  <$ char '\''
                      <|> "\"" <$ char '"'
                      <|> "?"  <$ char '?'

decimalLiteral :: Integral a => XParser a
decimalLiteral = token "decimal" decimal

signedDecLiteral :: Integral a => XParser a
signedDecLiteral = token "signed decimal" $ signed decimal

doubleLiteral ::  XParser Double
doubleLiteral = token "float" double
