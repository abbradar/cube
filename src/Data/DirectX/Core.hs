module Data.DirectX.Core where

import Data.Int
import Data.Word
import Data.Maybe
import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.CaseInsensitive as CI
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.CharSet as CS
import qualified Data.String as DS
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as U
import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token

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

newtype ParserState m = ParserState { templates :: Map TName (TemplateData, XParserT m Values)
                                    }

type XParsing m = (Monad m, MonadPlus m, TokenParsing m)
type XParserT m = StateT (ParserState m) m

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

data Dimension = Const Int
               | Ref MName
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

name :: XParsing m => m ByteString
name = do
  h <- oneOfSet firstSet
  t <- token $ many $ oneOfSet nextSet
  _ <- optional someSpace
  return $ B.pack (h:t)
  <?> "name"
  
  where firstSet = CS.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_']
        nextSet = CS.union firstSet $ CS.fromList ['0'..'9']

guid :: forall m. XParsing m => m UUID
guid = angles $
       fromJust <$> U.fromString <$> mconcat <$> sequence
       [ alnums 8, delim
       , alnums 4, delim
       , alnums 4, delim
       , alnums 4, delim
       , alnums 12
       ]
       <* optional someSpace
       <?> "guid"

  where alnums :: Int -> m String
        alnums n = mapM (const $ oneOfSet $ CS.fromList $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']) [1..n]
        delim = char '-' *> pure "-"

ciSymbol :: XParsing m => Text -> m Text
ciSymbol "" = "" <$ optional someSpace
ciSymbol t = do
  t' <- try $ T.pack <$> mapM (const anyChar) [1..T.length t]
  unless (CI.mk t == CI.mk t') $ fail "ciSymbol"
  _ <- optional someSpace
  return t
