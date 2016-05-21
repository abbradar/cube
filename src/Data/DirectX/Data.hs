module Data.DirectX.Data
       ( DName(..)
       , Data(..)
       , values
       , object
       ) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID.Types (UUID)
import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Parser.Token

import Data.DirectX.Core

-- Data name
newtype DName = DName ByteString
              deriving (Show, Eq, Ord)

data Data = Data { dataTemplate :: TName
                 , dataName :: Maybe DName
                 , dataGuid :: Maybe UUID
                 , dataValues :: Values
                 , dataChildren :: [Data]
                 }
          deriving (Show, Eq)

type ValueParser m = forall a. m a -> m (ValueData a)

one :: XParsing m => ValueParser m
one p = DV <$> p

array :: XParsing m => Int -> ValueParser m
array n p = do
  r <- commaSep p
  unless (length r == n) $ fail "array: invalid length"
  return $ DA r
  <?> "array"

number :: (Floating a, XParsing m) => m a
number = either fromIntegral (fromRational . toRational) <$> integerOrScientific

value :: XParsing m => ValueParser (XParserT m) -> ValueType -> XParserT m Value
value p Word = VWord <$> p (fromIntegral <$> natural)
value p DWord = VDWord <$> p (fromIntegral <$> natural)
value p Float = VFloat <$> p number
value p Double = VDouble <$> p number
value p Char = VChar <$> p (fromIntegral <$> integer)
value p UChar = VByte <$> p (fromIntegral <$> natural)
value p Byte = VByte <$> p (fromIntegral <$> natural)
value p String = VString <$> p stringLiteral
value p (Custom n) = do
  ts <- get
  VCustom <$> p (snd $ templates ts M.! n)

values :: XParsing m => TemplateData -> XParserT m Values
values tmpl = foldM (\ms (t, n) -> member ms n t <* semi) M.empty $ typeMembers tmpl
  where member ms n (Value v) = do
          r <- value one v
          return $ M.insert n r ms
          <?> "single value"
        member ms n (Array v dims) = do
          let totalDims = product $ map (realize ms) dims
          r <- value (array totalDims) v
          return $ M.insert n r ms
          <?> "array value"

        realize _ (Const i) = i
        realize ms (Ref n) = extractNum (ms M.! n)

        extractNum (VWord (DV i)) = fromIntegral i
        extractNum (VDWord (DV i)) = fromIntegral i
        extractNum (VChar (DV i)) = fromIntegral i
        extractNum (VByte (DV i)) = fromIntegral i
        extractNum _ = error "extractNum: impossible"

children :: XParsing m => Restriction -> XParserT m [Data]
children Closed = return []
children Opened = many object
children (Restricted allowed) = many $ do
  obj <- object
  unless (dataTemplate obj `S.member` allowed) $ fail "children: child data of this type is not allowed"
  return obj

object :: XParsing m => XParserT m Data
object = do
  dataTemplate <- TName <$> name
  ts <- get
  unless (dataTemplate `M.member` templates ts) $ fail "object: unknown type name"
  let (tdata, vparser) = templates ts M.! dataTemplate
  dataName <- optional $ DName <$> name
  braces $ do
    dataGuid <- optional guid
    dataValues <- vparser
    dataChildren <- children $ typeRestriction tdata
    return Data {..}
  <?> "object"
