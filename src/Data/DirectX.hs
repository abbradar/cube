module Data.DirectX
       ( XHeader(..)
       , XTemplates
       , DirectX(..)
       , directX
       , directX'
       ) where

import Prelude hiding (take)
import Data.Maybe
import Control.Applicative
import Control.Monad
import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import Control.Monad.Trans.State
import Control.Monad.Trans.Class
import Data.Attoparsec.ByteString.Char8

import Data.DirectX.Core
import Data.DirectX.Templates
import Data.DirectX.Data

data TopLevel = XTemplate (TName, TemplateData)
              | XData Data
              deriving (Show, Eq)

topLevel :: Bool -> XParser TopLevel
topLevel allowRedefinition =
      (XTemplate <$> template allowRedefinition)
  <|> (XData <$> object)

data XHeader = XHeader { majorVersion :: Int
                       , minorVersion :: Int
                       , formatType :: ByteString
                       , floatSize :: Int
                       }
             deriving (Show, Eq)

header :: Parser XHeader
header = do
  _ <- string "xof "
  majorVersion <- num 2
  minorVersion <- num 2
  formatType <- B.filter (/= ' ') <$> take 4
  floatSize <- num 4
  return XHeader {..}

  where num (sz :: Int) = do
          s <- B.filter (/= ' ') <$> take sz
          case readMaybe $ B.unpack s of
            Just n -> return n
            Nothing -> fail "num: invalid number"

type XTemplates = Map TName TemplateData

data DirectX = DirectX { xHeader :: XHeader
                       , xTemplates :: XTemplates
                       , xData :: [Data]
                       }
             deriving (Show, Eq)

directX :: Bool -> Parser DirectX
directX allowRedefinition = directX' allowRedefinition M.empty

directX' :: Bool -> XTemplates -> Parser DirectX
directX' allowRedefinition knownTemplates = do
  xHeader@(XHeader {..}) <- header
  unless (majorVersion == 3 && minorVersion == 3 && formatType == "txt" && floatSize == 32) $ fail "directX: unsupported format"

  let initState = ParserState { dxTemplates = fmap (\t -> (t, values t)) knownTemplates
                              , dxTemplateGuids = S.fromList $ map (\(_, t) -> typeGuid t) $ M.toList knownTemplates
                              , dxObjects = M.empty
                              , dxGuidObjects = M.empty
                              }

  flip evalStateT initState $ do
    skipSeparators
    topLevels <- many $ topLevel allowRedefinition
    lift endOfInput
    let xTemplates = M.fromList $ mapMaybe (\case XTemplate t -> Just t; XData _ -> Nothing) topLevels
    let xData = mapMaybe (\case XData d -> Just d; XTemplate _ -> Nothing) topLevels
    return DirectX {..}
