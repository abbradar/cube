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

import Data.DirectX.Core

braces :: XParser a -> XParser a
braces parser = symChar '{' *> parser <* symChar '}'

type ValueParser = forall a. XParser a -> XParser (ValueData a)

one :: ValueParser
one p = DV <$> p

array ::  Int -> ValueParser
array n p = do
  r <- p `sepBy` symChar ','
  unless (length r == n) $ fail "array: invalid length"
  return $ DA r

value :: ValueParser -> ValueType -> XParser Value
value p typ = case typ of
  Word -> VWord <$> p decimalLiteral
  DWord -> VDWord <$> p decimalLiteral
  Float -> VFloat <$> p (fromRational <$> toRational <$> doubleLiteral)
  Double -> VDouble <$> p doubleLiteral
  Char -> VChar <$> p signedDecLiteral
  UChar -> VByte <$> p decimalLiteral
  Byte -> VByte <$> p decimalLiteral
  String -> VString <$> p stringLiteral
  Custom n -> do
    ts <- get
    VCustom <$> p (snd $ dxTemplates ts M.! n)

values ::  TemplateData -> XParser Values
values tmpl = foldM (\ms (t, n) -> member ms n t <* symChar ';') M.empty $ typeMembers tmpl
  where member ms n (Value v) = do
          r <- value one v
          return $ M.insert n r ms
        member ms n (Array v dims) = do
          let totalDims = product $ map (realize ms) dims
          r <- value (array totalDims) v
          return $ M.insert n r ms

        realize _ (DConst i) = i
        realize ms (DRef n) = extractNum (ms M.! n)

        extractNum (VWord (DV i)) = fromIntegral i
        extractNum (VDWord (DV i)) = fromIntegral i
        extractNum (VChar (DV i)) = fromIntegral i
        extractNum (VByte (DV i)) = fromIntegral i
        extractNum _ = error "extractNum: impossible"

children :: Restriction -> XParser [Data]
children Closed = return []
children Opened = many childObject
children (Restricted allowed) = many $ do
  obj <- childObject
  unless (dataTemplate obj `S.member` allowed) $ fail "children: child data of this type is not allowed"
  return obj

childObject :: XParser Data
childObject = object <|> nameReference <|> guidReference

nameReference :: XParser Data
nameReference = braces $ do
  dname <- DName <$> name
  dguid <- optional guid
  ts <- get
  case (M.lookup dname $ dxObjects ts, dguid) of
    (Nothing, _) -> fail "nameReference: unknown name"
    (Just d, Just myid) | dataGuid d /= Just myid -> fail "nameReference: invalid GUID"
    (Just d, _) -> return d

guidReference ::  XParser Data
guidReference = braces $ do
  dguid <- guid
  ts <- get
  case M.lookup dguid $ dxGuidObjects ts of
    Nothing -> fail "guidReference: unknown guid"
    Just d -> return d

object ::  XParser Data
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
