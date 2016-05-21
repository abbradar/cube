module Data.DirectX
       ( XHeader(..)
       , XTemplates
       , DirectX(..)
       , directX
       , directX'
       ) where

import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.State
import Text.Parser.Combinators
import Text.Parser.Char
import Text.Parser.Token

import Text.Parser.Comments
import Data.DirectX.Core
import Data.DirectX.Templates
import Data.DirectX.Data

type XIParser a = forall m. XParsing m => XParserT (LineCommentT "#" (LineCommentT "//" m)) a

data TopLevel = XTemplate (TName, TemplateData)
              | XData Data
              deriving (Show, Eq)

topTemplate :: XIParser (TName, TemplateData)
topTemplate = do
  (tn, t) <- template
  ts <- get
  when (tn `M.member` templates ts) $ fail "topTemplate: repeating template name"
  put ts { templates = M.insert tn (t, values t) $ templates ts }
  return (tn, t)

topLevel :: XIParser TopLevel
topLevel =     (XTemplate <$> topTemplate)
           <|> (XData <$> object)

data XHeader = XHeader { majorVersion :: Int
                       , minorVersion :: Int
                       , formatType :: ByteString
                       , floatSize :: Int
                       }
             deriving (Show, Eq)

header :: XParsing m => m XHeader
header = do
  _ <- text "xof "
  majorVersion <- num 2
  minorVersion <- num 2
  formatType <- B.pack <$> filter (/= ' ') <$> mapM (const anyChar) [1..4::Int]
  floatSize <- num 4
  return XHeader {..}

  where num (sz :: Int) = do
          s <- dropWhile (== ' ') <$> mapM (const anyChar) [1..sz]
          case readMaybe s of
            Just n -> return n
            Nothing -> fail "num: invalid number"

type XTemplates = Map TName TemplateData

data DirectX = DirectX { xHeader :: XHeader
                       , xTemplates :: XTemplates
                       , xData :: [Data]
                       }
             deriving (Show, Eq)

directX :: XParsing m => m DirectX
directX = directX' M.empty

directX' :: XParsing m => XTemplates -> m DirectX
directX' knownTemplates = do
  xHeader@(XHeader {..}) <- header
  unless (majorVersion == 3 && minorVersion == 3 && formatType == "txt" && floatSize == 32) $ fail "directX: unsupported format"

  let initState = ParserState { templates = fmap (\t -> (t, values t)) knownTemplates }

  runLineCommentT $ runLineCommentT $ flip evalStateT initState $ do
    _ <- optional someSpace
    topLevels <- many topLevel
    eof
    let xTemplates = M.fromList $ mapMaybe (\case XTemplate t -> Just t; XData _ -> Nothing) topLevels
    let xData = mapMaybe (\case XData d -> Just d; XTemplate _ -> Nothing) topLevels
    return DirectX {..}
