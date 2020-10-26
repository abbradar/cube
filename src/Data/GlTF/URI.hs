-- | Parsers for GlTF URIs conforming RFC 3986, RFC 2397.

{-# LANGUAGE StrictData #-}

module Data.GlTF.URI
  ( Type
  , Subtype
  , Attribute
  , Value
  , TypePair
  , GlTFDataURI(..)
  , GlTFURI(..)
  , gltfUriParser
  , decodeGltfUri

  , MediaType(..)
  , mediaTypeParser
  , decodeMediaType
  ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64.Lazy as Base64L
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Lazy.Encoding as TL
import Data.Attoparsec.Text

type Type = Text
type Subtype = Text
type Attribute = Text
type Value = Text

type TypePair = (Type, Subtype)

data MediaType = MediaType { typePair :: TypePair
                           , mediaParameters :: HashMap Attribute Value
                           }
               deriving (Show, Eq)

data GlTFDataURI = GlTFDataURI { uriMediaType :: Maybe MediaType
                               , uriData :: ByteString
                               }
             deriving (Show, Eq)

data GlTFURI = GlFileURI FilePath
             | GlDataURI GlTFDataURI
             deriving (Show, Eq)

alphaClass :: String
alphaClass = ['a'..'z'] ++ ['A'..'Z']

digitClass :: String
digitClass = ['0'..'9']

hexdigClass :: String
hexdigClass = digitClass ++ ['A'..'F']

unreservedClass :: String
unreservedClass = ['-', '.', '_', '~'] ++ alphaClass ++ digitClass

subDelimsClass :: String
subDelimsClass = ['!', '$', '&', '\'', '(', ')', '*', '+', ',', ';', '=']

mimeTokenClass :: String
mimeTokenClass = ['-', '!', '#', '$', '%', '&', '\'', '*', '+', '.', '^', '_', '`', '{', '|', '}', '~'] ++ alphaClass ++ digitClass

segmentNzNcClass :: String
segmentNzNcClass = unreservedClass ++ subDelimsClass ++ ['@']

pcharClass :: String
pcharClass = segmentNzNcClass ++ [':']

urlcClass :: String
urlcClass = unreservedClass ++ [';', '?', ':', '@', '&', '=', '+', '$', ',', '/']

hexToChar :: Char -> Char -> Char
hexToChar a b = toEnum (16 * conv a + conv b)
  where conv c
          | c >= '0' && c <= '9' = fromEnum c - fromEnum '0'
          | c >= 'A' && c <= 'F' = fromEnum c - fromEnum 'A'
          | otherwise = error "Invalid hex chars"

anyPctEncoded :: Parser Char
anyPctEncoded = char '%' *> (hexToChar <$> hexdig <*> hexdig)
  where hexdig = satisfy (inClass hexdigClass)

pctEncoded :: (Char -> Bool) -> Parser Char
pctEncoded check = do
  c <- anyPctEncoded
  unless (check c) $ fail "Invalid character"
  return c

-- | Parse *pchar
reallyGenericPstring :: (Parser TLB.Builder -> Parser [TLB.Builder]) -> [Char] -> (Char -> Bool) -> Parser TLB.Builder
reallyGenericPstring combinator charClass check = mconcat <$> combinator (TLB.fromText <$> takeWhile1 (\x -> inClass charClass x && check x) <|> (TLB.singleton <$> pctEncoded check))

genericPstring :: [Char] -> (Char -> Bool) -> Parser TLB.Builder
genericPstring = reallyGenericPstring many

genericPstring1 :: [Char] -> (Char -> Bool) -> Parser TLB.Builder
genericPstring1 = reallyGenericPstring some

-- | Parse *pchar
pstring :: (Char -> Bool) -> Parser TLB.Builder
pstring = genericPstring pcharClass

anyPstring :: Parser TLB.Builder
anyPstring = pstring (const True)

scheme :: Parser Text
scheme = T.cons <$> satisfy (inClass alphaClass) <*> takeWhile (inClass schemeClass)
  where schemeClass = alphaClass ++ digitClass ++ ['+', '-', '.']

gltfUriParser :: Parser GlTFURI
gltfUriParser = gltfSchemeUri <|> (GlFileURI <$> gltfPathNoschemeUri)

segmentNzNc :: Parser TLB.Builder
segmentNzNc = genericPstring1 segmentNzNcClass (const True)

segment :: Parser TLB.Builder
segment = anyPstring

urlString :: Parser TLB.Builder
urlString = genericPstring urlcClass (const True)

gltfPathNoschemeUri :: Parser FilePath
gltfPathNoschemeUri = do
  firstSeg <- segmentNzNc
  parts <- many $ char '/' *> segment
  let path = firstSeg <> mconcat (map (TLB.singleton '/' <>) parts)
  return $ TL.unpack $ TLB.toLazyText path

uriMimeToken :: Parser Text
uriMimeToken = TL.toStrict <$> TLB.toLazyText <$> pstring (inClass mimeTokenClass)

uriMediaTypePair :: Parser (Type, Subtype)
uriMediaTypePair = do
  typ <- T.toLower <$> uriMimeToken
  _ <- char '/'
  subtyp <- T.toLower <$> uriMimeToken
  return (typ, subtyp)

uriMediaParameter :: Parser (Attribute, Value)
uriMediaParameter = do
  attr <- T.toLower <$> uriMimeToken
  _ <- char '='
  value <- uriMimeToken
  return (attr, value)

uriMediaTypeParser :: Parser MediaType
uriMediaTypeParser = do
  typePair <- uriMediaTypePair
  mediaParameters <- HMS.fromList <$> many (char ';' *> uriMediaParameter)
  return MediaType {..}

gltfDataUri :: Parser GlTFDataURI
gltfDataUri = do
  uriMediaType <- optional uriMediaTypeParser
  isBase64 <- optional (string ";base64")
  _ <- char ','
  dataStr <- TL.encodeUtf8 <$> TLB.toLazyText <$> urlString
  decodedStr <-
    case isBase64 of
      Just _ ->
        case Base64L.decode dataStr of
          Right decodedStr -> return decodedStr
          Left e -> fail e
      Nothing -> return dataStr
  return GlTFDataURI { uriMediaType, uriData = BL.toStrict decodedStr }

gltfSchemeUri :: Parser GlTFURI
gltfSchemeUri = do
  uriScheme <- scheme <* char ':'
  case uriScheme of
    "data" -> GlDataURI <$> gltfDataUri
    _ -> fail $ "Unknown schema: " ++ T.unpack uriScheme

decodeGltfUri :: Text -> Either String GlTFURI
decodeGltfUri = parseOnly (gltfUriParser <* endOfInput)

--
-- MIME media type parsing.
--

mimeToken :: Parser Text
mimeToken = takeWhile1 (inClass mimeTokenClass)

mediaTypeSubtype :: Parser (Type, Subtype)
mediaTypeSubtype = do
  typ <- mimeToken
  _ <- char '/'
  subtyp <- mimeToken
  return (typ, subtyp)

mimeValue :: Parser Text
mimeValue = mimeToken <|> mimeQuotedString

mimeQuotedString :: Parser Text
mimeQuotedString = char '"' *> (TL.toStrict <$> TLB.toLazyText <$> mconcat <$> many (insideString <|> quotedChar)) <* char '"'
  where insideString = TLB.fromText <$> takeWhile1 (\x -> x /= '"' && x /= '\\' && x /= '\n')
        quotedChar = char '\\' *> (TLB.singleton <$> anyChar)

mediaParameter :: Parser (Attribute, Value)
mediaParameter = do
  attr <- T.toLower <$> mimeToken
  _ <- char '='
  value <- mimeValue
  return (attr, value)

mediaTypeParser :: Parser MediaType
mediaTypeParser = do
  typePair <- mediaTypeSubtype
  mediaParameters <- HMS.fromList <$> many (char ';' *> mediaParameter)
  return MediaType {..}

decodeMediaType :: Text -> Either String MediaType
decodeMediaType = parseOnly (mediaTypeParser <* endOfInput)
