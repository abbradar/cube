-- | Shader cache allowing

{-# LANGUAGE StrictData #-}

module Cube.Graphics.ShadersCache
  ( AttributeName
  , UniformName
  , ShaderDefinitions
  , LoadedPipeline(..)
  , PipelineCache
  , newPipelineCache
  , getOrCompilePipeline
  ) where

import Data.Typeable
import Control.Monad
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.String.Interpolate
import Control.Monad.Logger
import Control.Monad.Catch
import Graphics.Caramia
import Graphics.Caramia.OpenGLResource

import Data.GLSL.Preprocessor
import Data.WeakCache (WeakCache)
import qualified Data.WeakCache as WeakCache
import Cube.Types

type AttributeName = ByteString
type UniformName = ByteString
type ShaderDefinitions = HashMap MacroName (Maybe MacroDefinition)

data LoadedPipeline = LoadedPipeline { loadedPipeline :: Pipeline
                                     , loadedPipelineId :: Int -- Used for fast indexing
                                     , loadedAttributes :: HashMap AttributeName (AttributeLocation, AttributeInfo)
                                     , loadedUniforms :: HashMap UniformName (UniformLocation, UniformInfo)
                                     }

data PipelineCache meta = PipelineCache { pipelineVertex :: ShaderWithIncludes
                                        , pipelineFragment :: ShaderWithIncludes
                                        , pipelinePostLink :: LoadedPipeline -> Either String meta
                                        , pipelineCache :: WeakCache ShaderDefinitions (meta, LoadedPipeline)
                                        }

newPipelineCache :: MonadCube m => (LoadedPipeline -> Either String meta) -> ShaderWithIncludes -> ShaderWithIncludes -> m (PipelineCache meta)
newPipelineCache pipelinePostLink pipelineVertex pipelineFragment = do
  pipelineCache <- WeakCache.new
  return PipelineCache {..}

data PreprocessedShaderCompilationError = PreprocessedShaderCompilationError ShaderPaths Text
                                        deriving (Show, Eq, Typeable, Exception)

-- Returns False as second argument if a cached pipeline was used, True if a new one has been built.
getOrCompilePipeline :: MonadCube m => ShaderDefinitions -> PipelineCache meta -> m (meta, LoadedPipeline)
getOrCompilePipeline defns (PipelineCache {..}) = WeakCache.getOrCreate defns create pipelineCache
  where create = do
          let compileOne stage shader = do
                let source = shaderSource (HMS.toList defns) shader
                ret <- catch (newShaderB source stage) (\(ShaderCompilationError txt) -> throwM $ PreprocessedShaderCompilationError (shaderPaths shader) txt)
                logTxt <- getShaderLog ret
                unless (T.null logTxt) $ $(logWarn) [i|Warnings during shader compilation:\n#{logTxt}|]
                return ret
          vert <- compileOne Vertex pipelineVertex
          frag <- compileOne Fragment pipelineFragment
          loadedPipeline <- newPipeline [vert, frag] mempty
          logTxt <- getPipelineLog loadedPipeline
          unless (T.null logTxt) $ $(logWarn) [i|Warnings during shader linking:\n#{logTxt}|]
          loadedAttributes <- HMS.fromList <$> zipWith (\idx info -> (attributeName info, (idx, info))) [0..] <$> getActiveAttributes loadedPipeline
          loadedUniforms <- HMS.fromList <$> zipWith (\idx info -> (uniformName info, (idx, info))) [0..] <$> getActiveUniforms loadedPipeline
          loadedPipelineId <- fromIntegral <$> getRaw loadedPipeline
          let pl = LoadedPipeline {..}
          case pipelinePostLink pl of
            Left e -> fail $ "Failed pipeline post-link step: " ++ e
            Right meta -> return (meta, pl)
