module Data.DirectX.Templates
       ( template
       ) where

import Control.Applicative hiding (Const)
import Control.Monad
import Data.ByteString (ByteString)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.UUID.Types (UUID)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Parser.Token

import Data.DirectX.Core

type XTParser a = forall m. XParsing m => StateT (Map MName Type) (XParserT m) a

guid' :: XTParser UUID
guid' = lift guid

name' :: XTParser ByteString
name' = lift name

valueType :: XTParser ValueType
valueType = foldr ((<|>) . ts) mzero
        [ ("WORD", Word)
        , ("DWORD", DWord)
        , ("FLOAT", Float)
        , ("DOUBLE", Double)
        , ("CHAR", Char)
        , ("UCHAR", UChar)
        , ("BYTE", Byte)
        , ("STRING", String)
        ]
        <|> (Custom <$> TName <$> name')
        <?> "value type"
  where ts (s, t) = try (t <$ ciSymbol s)

refDimension :: XTParser MName
refDimension = do
  n <- MName <$> name'
  ns <- get
  case M.lookup n ns of
    Nothing -> fail "refDimension: reference to an unknown member name"
    Just (Value Word) -> return ()
    Just (Value DWord) -> return ()
    Just (Value Char) -> return ()
    Just (Value UChar) -> return ()
    Just (Value Byte) -> return ()
    _ -> fail "refDimension: reference to a non-integral member in array dimension"
  return n

dimension :: XTParser Dimension
dimension = brackets $     (Const <$> fromIntegral <$> natural)
                       <|> (Ref <$> refDimension)
            <?> "dimension"

arrayMember :: XTParser (Type, MName)
arrayMember = do
  _ <- try $ textSymbol "array"
  t <- valueType
  n <- MName <$> name'
  ds <- some dimension
  return (Array t ds, n)
  <?> "array member"

valueMember :: XTParser (Type, MName)
valueMember = do
  t <- valueType
  n <- MName <$> name'
  return (Value t, n)
  <?> "value member"
  
member :: XTParser (Type, MName)
member = do
  r@(t, n) <- arrayMember <|> valueMember
  ns <- get
  when (n `M.member` ns) $ fail "member: repeating member name"
  put $ M.insert n t ns
  return r
  <?> "member"

-- XXX: this ignores GUID
restrictedName :: XTParser TName
restrictedName = do
  n <- TName <$> name' <* optional guid'
  tns <- lift get
  unless (n `M.member` templates tns) $ fail "restrictedName: reference to an unknown template name"
  return n

restriction :: XTParser Restriction
restriction = brackets ((Opened <$ textSymbol "...") <|> (Restricted <$> S.fromList <$> commaSep1 restrictedName))
              <|> pure Closed
              <?> "restriction"

template :: XParsing m => XParserT m (TName, TemplateData)
template = do
  _ <- try $ textSymbol "template"
  tname <- TName <$> name
  braces $ flip evalStateT M.empty $ do
    typeGuid <- guid'
    typeMembers <- many (member <* semi)
    typeRestriction <- restriction
    return (tname, TemplateData {..})
  <?> "template"
