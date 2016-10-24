module Data.DirectX.Data
       ( DName(..)
       , Data(..)
       , values
       , object
       ) where

import Control.Monad
import Control.Applicative
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Parser.Token

import Data.DirectX.Core

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
  VCustom <$> p (snd $ dxTemplates ts M.! n)

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

        realize _ (DConst i) = i
        realize ms (DRef n) = extractNum (ms M.! n)

        extractNum (VWord (DV i)) = fromIntegral i
        extractNum (VDWord (DV i)) = fromIntegral i
        extractNum (VChar (DV i)) = fromIntegral i
        extractNum (VByte (DV i)) = fromIntegral i
        extractNum _ = error "extractNum: impossible"

children :: XParsing m => Restriction -> XParserT m [Data]
children Closed = return []
children Opened = many childObject
children (Restricted allowed) = many $ do
  obj <- childObject
  unless (dataTemplate obj `S.member` allowed) $ fail "children: child data of this type is not allowed"
  return obj

childObject :: XParsing m => XParserT m Data
childObject = object <|> nameReference <|> guidReference

nameReference :: XParsing m => XParserT m Data
nameReference = braces $ do
  dname <- DName <$> name
  dguid <- optional guid
  ts <- get
  case (M.lookup dname $ dxObjects ts, dguid) of
    (Nothing, _) -> fail "nameReference: unknown name"
    (Just d, Just myid) | dataGuid d /= Just myid -> fail "nameReference: invalid GUID"
    (Just d, _) -> return d
  <?> "nameReference"

guidReference :: XParsing m => XParserT m Data
guidReference = braces $ do
  dguid <- guid
  ts <- get
  case M.lookup dguid $ dxGuidObjects ts of
    Nothing -> fail "guidReference: unknown guid"
    Just d -> return d
  <?> "guidReference"

object :: XParsing m => XParserT m Data
object = do
  dataTemplate <- TName <$> name
  (tdata, vparser) <- do
    ts <- get
    unless (dataTemplate `M.member` dxTemplates ts) $ fail "object: unknown type name"
    return $ dxTemplates ts M.! dataTemplate
  dataName <- optional $ DName <$> name
  braces $ do
    dataGuid <- optional guid
    dataValues <- vparser
    dataChildren <- children $ typeRestriction tdata
    let res = Data {..}
    case dataName of
      Nothing -> return ()
      Just dt -> do
        ts <- get
        when (dt `M.member` dxObjects ts) $ fail "object: duplicate name"
        put ts { dxObjects = M.insert dt res $ dxObjects ts }
    case dataGuid of
      Nothing -> return ()
      Just dguid -> do
        ts <- get
        when (dguid `M.member` dxGuidObjects ts) $ fail "object: duplicate GUID"
        put ts { dxGuidObjects = M.insert dguid res $ dxGuidObjects ts }
    return res
  <?> "object"
