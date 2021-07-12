-- | Preprocessor for shaders handling #include directives.

{-# LANGUAGE StrictData #-}

module Data.GLSL.Preprocessor
  ( SourceStringId
  , ShaderPaths
  , ShaderWithIncludes(..)
  , readAndPreprocessShader
  , MacroName
  , MacroDefinition
  , shaderSource
  ) where

import Data.Functor
import Control.Applicative
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TLB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.String.Interpolate
import Control.Monad.IO.Class
import Control.Monad.State.Strict
import Control.Monad.Except
import System.FilePath
import System.Directory
import qualified Data.Attoparsec.Text as Atto

data ShaderProfile = Core | Compatibility | ES
                   deriving (Show, Eq)

data ExtensionBehaviour = Require | Enable | Warn | Disable
                        deriving (Show, Eq)

data ShaderExtension = ExtAll | ExtName Text
                     deriving (Show, Eq)

data IncludeInfo = IncludeInfo { includeLocalFirst :: Bool
                               , includePath :: FilePath
                               }
                 deriving (Show, Eq)

data VersionInfo = VersionInfo { versionNumber :: Int
                               , versionProfile :: Maybe ShaderProfile
                               }
                 deriving (Show, Eq)

data ExtensionInfo = ExtensionInfo { extensionName :: ShaderExtension
                                   , extensionBehaviour :: ExtensionBehaviour
                                   }
                   deriving (Show, Eq)

data ShaderDirective = IncludeDirective IncludeInfo
                     | VersionDirective VersionInfo
                     | ExtensionDirective ExtensionInfo
                     deriving (Show, Eq)

data ShaderLine = EmptyLine
                | DirectiveLine ShaderDirective
                deriving (Show, Eq)

includeParser :: Atto.Parser IncludeInfo
includeParser = do
  _ <- Atto.asciiCI "include"
  Atto.skipSpace
  (includeLocalFirst, rawIncludePath) <-
        (Atto.char '<' *> ((False, ) <$> Atto.takeWhile1 (/= '>')) <* Atto.char '>')
    <|> (Atto.char '"' *> ((True, ) <$> Atto.takeWhile1 (/= '"')) <* Atto.char '"')
  let includePath = T.unpack rawIncludePath
  Atto.skipSpace
  return IncludeInfo {..}

versionParser :: Atto.Parser VersionInfo
versionParser = do
  _ <- Atto.asciiCI "version"
  Atto.skipSpace
  versionNumber <- Atto.decimal
  Atto.skipSpace
  versionProfile <- optional $
        (Atto.asciiCI "core" $> Core)
    <|> (Atto.asciiCI "compatibility" $> Compatibility)
    <|> (Atto.asciiCI "es" $> ES)
  Atto.skipSpace
  return VersionInfo {..}

extensionParser :: Atto.Parser ExtensionInfo
extensionParser = do
  _ <- Atto.asciiCI "extension"
  Atto.skipSpace
  extensionName <-
        (Atto.asciiCI "all" $> ExtAll)
    <|> (ExtName <$> Atto.takeWhile1 (/= ' '))
  Atto.skipSpace
  extensionBehaviour <-
        (Atto.asciiCI "require" $> Require)
    <|> (Atto.asciiCI "enable" $> Enable)
    <|> (Atto.asciiCI "warn" $> Warn)
    <|> (Atto.asciiCI "disable" $> Disable)
  Atto.skipSpace
  return ExtensionInfo {..}

shaderDirectiveParser :: Atto.Parser ShaderDirective
shaderDirectiveParser = Atto.char '#' *> Atto.skipSpace *> dirs <* Atto.endOfInput
  where dirs = (IncludeDirective <$> includeParser) <|> (VersionDirective <$> versionParser) <|> (ExtensionDirective <$> extensionParser)

shaderLineParser :: Atto.Parser ShaderLine
shaderLineParser = Atto.skipSpace *> ((Atto.endOfInput $> EmptyLine) <|> (DirectiveLine <$> shaderDirectiveParser))

type SourceLine = Int
type SourceStringId = Int

data EncounteredSources = EncounteredSources { encSources :: Map FilePath (Maybe (SourceStringId, TLB.Builder))
                                             , encNextId :: SourceStringId
                                             , encExtensions :: [Text]
                                             , encVersion :: Maybe Text
                                             }
                          deriving (Show, Eq)

buildPreamble :: EncounteredSources -> ByteString
buildPreamble (EncounteredSources {..}) = T.encodeUtf8 $ TL.toStrict $ TLB.toLazyText versionLine
  where extLines = foldr addLine mempty encExtensions
        versionLine = maybe extLines (\line -> addLine line extLines) encVersion

initialEncounteredSources :: EncounteredSources
initialEncounteredSources = EncounteredSources { encSources = M.empty
                                               , encNextId = 0
                                               , encExtensions = []
                                               , encVersion = Nothing
                                               }

addLine :: Text -> TLB.Builder -> TLB.Builder
addLine line otherLines = TLB.fromText line <> TLB.singleton '\n' <> otherLines

preprocessOneShader :: MonadIO m => Bool -> FilePath -> StateT EncounteredSources (ExceptT String m) TLB.Builder
preprocessOneShader topLevel path = do
  path' <- liftIO $ canonicalizePath path
  encountered <- get
  case M.lookup path' $ encSources encountered of
    Just (Just (_prevId, prevContents)) -> return prevContents
    Just Nothing -> throwError "Include cycle detected"
    Nothing -> do
      let newId = encNextId encountered
      put encountered { encNextId = newId + 1
                      , encSources = M.insert path' Nothing $ encSources encountered
                      }

      let relativePath = takeDirectory path'

          buildOutput (lineNum :: SourceLine, line) srcLines = do
            srcs@EncounteredSources {..} <- get
            case Atto.parseOnly shaderLineParser line of
              Left _ | [] <- encExtensions, Nothing <- encVersion -> return $ line `addLine` srcLines
              Right EmptyLine -> return $ TLB.singleton '\n' <> srcLines
              Right (DirectiveLine (IncludeDirective (IncludeInfo {..}))) -> do
                included <- preprocessOneShader False (relativePath </> includePath)
                let lineDirective = [i|\n\#line #{lineNum + 1} #{newId}\n|]
                return $ included <> lineDirective <> srcLines
              Right (DirectiveLine (VersionDirective _verInfo)) | topLevel, Nothing <- encVersion -> do
                put srcs { encVersion = Just line }
                return $ TLB.singleton '\n' <> srcLines
              Right (DirectiveLine (ExtensionDirective _extInfo)) | topLevel, Nothing <- encVersion -> do
                put srcs { encExtensions = line : encExtensions }
                return $ TLB.singleton '\n' <> srcLines
              _ -> throwError "#version and #extension should occur in this order and before any other statements"

      contents <- liftIO $ T.readFile path'
      fileLines <- foldrM buildOutput mempty $ zip [1..] $ T.lines contents
      let initialLine = [i|\#line 1 #{newId}\n|]
          ret = initialLine <> fileLines
      modify (\x -> x { encSources = M.insert path' (Just (newId, ret)) $ encSources x })
      return ret

type ShaderPaths = HashMap SourceStringId FilePath

data ShaderWithIncludes = ShaderWithIncludes { shaderSources :: ByteString
                                             , shaderPaths :: ShaderPaths
                                             , shaderPreamble :: ByteString
                                             }
                        deriving (Show, Eq)

readAndPreprocessShader :: MonadIO m => FilePath -> m (Either String ShaderWithIncludes)
readAndPreprocessShader path = do
  ret <- runExceptT $ flip runStateT initialEncounteredSources $ preprocessOneShader True path
  let mapRet (sources, srcs@EncounteredSources {..}) =
        ShaderWithIncludes { shaderSources = T.encodeUtf8 $ TL.toStrict $ TLB.toLazyText sources
                           , shaderPaths = HMS.fromList $ map (\(srcPath, (Just (srcId, _))) -> (srcId, srcPath)) $ M.toList encSources
                           , shaderPreamble = buildPreamble srcs
                           }
  return $ fmap mapRet ret

type MacroName = ByteString
type MacroDefinition = ByteString

shaderSource :: [(MacroName, Maybe MacroDefinition)] -> ShaderWithIncludes -> ByteString
shaderSource defns (ShaderWithIncludes {..}) = BL.toStrict $ BB.toLazyByteString sourceFull
  where addDefinition (name, Nothing) = ([i|\#define #{name}\n|] <>)
        addDefinition (name, Just value) = ([i|\#define #{name} #{value}\n|] <>)

        sourceBody = foldr addDefinition (BB.byteString shaderSources) defns
        sourceFull = BB.byteString shaderPreamble <> sourceBody
