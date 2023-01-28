-- | Shader cache allowing

{-# LANGUAGE StrictData #-}

module Cube.Graphics.ShadersCache
  ( AttributeName
  , UniformName
  , ShaderDefinitions
  , ToShaderDefinitions(..)
  , PipelineId
  , LoadedPipeline(..)
  , PipelineCache
  , newPipelineCache
  , getOrCompilePipeline
  ) where

import Data.Maybe
import Data.Typeable
import Data.Hashable
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

import Data.GLSL.Preprocessor
import Data.WeakCache (WeakCache)
import qualified Data.WeakCache as WeakCache
import Cube.Types

type AttributeName = ByteString
type UniformName = ByteString
type ShaderDefinitions = HashMap MacroName (Maybe MacroDefinition)

type PipelineId = Int

class ToShaderDefinitions a where
  toShaderDefinitions :: a -> ShaderDefinitions

data LoadedPipeline = LoadedPipeline { loadedPipeline :: Pipeline
                                     , loadedAttributes :: HashMap AttributeName (AttributeLocation, AttributeInfo)
                                     , loadedUniforms :: HashMap UniformName (UniformLocation, UniformInfo)
                                     }
instance Show LoadedPipeline
  where show LoadedPipeline{..} = "attributes: " ++ show loadedAttributes ++ ",\n uniforms" ++ show loadedUniforms

data PipelineCache key meta = PipelineCache { pipelineVertex :: ShaderWithIncludes
                                            , pipelineFragment :: ShaderWithIncludes
                                            , pipelineCache :: WeakCache key (meta, LoadedPipeline)
                                            }

newPipelineCache :: MonadCube m => ShaderWithIncludes -> ShaderWithIncludes -> m (PipelineCache key meta)
newPipelineCache pipelineVertex pipelineFragment = do
  pipelineCache <- WeakCache.new
  return PipelineCache {..}

data PreprocessedShaderCompilationError = PreprocessedShaderCompilationError ShaderPaths Text
                                        deriving (Show, Eq, Typeable, Exception)

-- Returns False as second argument if a cached pipeline was used, True if a new one has been built.
getOrCompilePipeline :: (Eq key, Hashable key, ToShaderDefinitions key, MonadCube m) => (LoadedPipeline -> m meta) -> key -> PipelineCache key meta -> m (meta, LoadedPipeline)
getOrCompilePipeline postLink key (PipelineCache {..}) = WeakCache.getOrCreate key create pipelineCache
  where create = do
          let compileOne stage shader = do
                let source = shaderSource (HMS.toList $ toShaderDefinitions key) shader
                ret <- catch (newShaderB source stage) (\(ShaderCompilationError txt) -> throwM $ PreprocessedShaderCompilationError (shaderPaths shader) txt)
                logTxt <- getShaderLog ret
                unless (T.null logTxt) $ $(logWarn) [i|Warnings during shader compilation:\n#{logTxt}|]
                return ret
          vert <- compileOne Vertex pipelineVertex
          frag <- compileOne Fragment pipelineFragment
          loadedPipeline <- newPipeline [vert, frag] mempty
          logTxt <- getPipelineLog loadedPipeline
          unless (T.null logTxt) $ $(logWarn) [i|Warnings during shader linking:\n#{logTxt}|]
          attrs <- getActiveAttributes loadedPipeline
          attrLocs <- mapM (\info -> fromJust <$> getAttributeLocation (attributeName info) loadedPipeline) attrs
          let loadedAttributes = HMS.fromList $ zipWith (\idx info -> (attributeName info, (idx, info))) attrLocs attrs
          uniforms <- getActiveUniforms loadedPipeline
          uniformLocs <- mapM (\info -> fromJust <$> getUniformLocation (uniformName info) loadedPipeline) uniforms
          let loadedUniforms = HMS.fromList $ zipWith (\idx info -> (uniformName info, (idx, info))) uniformLocs uniforms
          let pl = LoadedPipeline {..}
          meta <- postLink pl
          return (meta, pl)
