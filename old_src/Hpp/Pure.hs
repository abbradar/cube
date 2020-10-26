-- | Pure interface for Hpp for running with virtual file systems.

module Hpp.Pure
  ( HppVirtualFS(..)
  , R.preprocess
  , processVirtualHpp
  , streamVirtualHpp
  , runVirtualHppM
  ) where

import System.FilePath ((</>))
import Data.ByteString (ByteString)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified Control.Monad.Trans.Writer.Strict as Wr
import Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as St
import qualified Hpp
import Hpp.Parser (Parser, evalParse)
import Hpp.Types hiding (String)
import Hpp.Config (includePaths, curFileName)
import Hpp.String (stripAngleBrackets)
import qualified Hpp.RunHpp as R

data HppVirtualFS m = HppVirtualFS { hppReadLines :: FilePath -> m [ByteString]
                                   , hppDoesFileExist :: FilePath -> m Bool
                                   }

liftVirtualFS :: (MonadTrans t, Monad m) => HppVirtualFS m -> HppVirtualFS (t m)
liftVirtualFS vfs = HppVirtualFS { hppReadLines = lift . hppReadLines vfs
                                 , hppDoesFileExist = lift . hppDoesFileExist vfs
                                 }

-- These are copied from Hpp sources and modified to use HppVirtualFS.

processVirtualHpp :: Monad m
                  => HppState
                  -> HppVirtualFS m
                  -> m [ByteString]
                  -> ExceptT Error m (Hpp.HppOutput, HppState)
processVirtualHpp state vfs readNewLines = do
  (retOrError, retLines) <- lift $ Wr.runWriterT $ runExceptT $
                            streamVirtualHpp state (liftVirtualFS vfs) Wr.tell (lift readNewLines)
  (paths, state') <- except retOrError
  let output = Hpp.HppOutput { hppFilesRead = paths
                             , hppOutput = retLines
                             }
  return (output, state')

streamVirtualHpp :: forall m. Monad m
                 => HppState
                 -> HppVirtualFS m
                 -> ([ByteString] -> m ())
                 -> m [ByteString]
                 -> ExceptT Error m ([FilePath], HppState)
streamVirtualHpp state vfs sink readNewLines = do
  (mret, state') <- flip St.runStateT state $ flip evalParse [] $ runVirtualHppM (liftVirtualFS $ liftVirtualFS $ liftVirtualFS vfs) (lift . lift . lift . sink) $
                    let tryRun :: HppT [ByteString] (Parser (StateT HppState (ExceptT Error m)) [TOKEN]) ()
                        tryRun = do
                          rs <- lift $ lift $ lift $ lift readNewLines
                          case rs of
                            [] -> return ()
                            _ -> do
                              R.preprocess rs
                              tryRun
                     in tryRun

  case mret of
    Left (_, e) -> throwE e
    Right ret -> return (R.hppFilesRead ret, state')

includeCandidates :: FilePath -> [FilePath] -> String -> Maybe [FilePath]
includeCandidates curDir searchPath nm =
  case nm of
    '<':nm' -> Just $ sysSearch (init nm')
    '"':nm' -> let nm'' = init nm'
               in Just $ nm'' : localSearch nm''
    _ -> Nothing
  where sysSearch   f = map (</> f) searchPath
        localSearch f = map (</> f) $ curDir : searchPath

searchForInclude :: Monad m => HppVirtualFS m -> FilePath -> [FilePath] -> String -> m (Maybe FilePath)
searchForInclude vfs curDir paths =
  maybe (return Nothing) aux . includeCandidates curDir paths
  where aux [] = return Nothing
        aux (f:fs) = do
          exists <- hppDoesFileExist vfs f
          if exists then return (Just f) else aux fs

searchForNextInclude :: Monad m => HppVirtualFS m -> FilePath -> [FilePath] -> String -> m (Maybe FilePath)
searchForNextInclude vfs curDir paths =
  maybe (return Nothing) (aux False) . includeCandidates curDir paths
  where aux _ [] = return Nothing
        aux n (f:fs) = do
          exists <- hppDoesFileExist vfs f
          if exists
          then if n
               then return (Just f)
               else aux True fs
          else aux n fs

runVirtualHppM :: forall m a. (Monad m, HasHppState m)
               => HppVirtualFS m
               -> ([ByteString] -> m ())
               -> HppT [ByteString] m a
               -> m (Either (FilePath, Error) (R.HppResult a))
runVirtualHppM vfs sink m = runHppT m >>= go []
  where go :: [FilePath]
           -> FreeF (HppF [ByteString]) a (HppT [ByteString] m a)
           -> m (Either (FilePath, Error) (R.HppResult a))
        go files (PureF x) = return $ Right (R.HppResult files x)
        go files (FreeF s) = case s of
          ReadFile ln file k -> do
            cfg    <- use config
            curDir <- use dir
            let ipaths = includePaths cfg
            mFound <- searchForInclude vfs curDir ipaths file
            readAux (file:files) ln file k mFound
          ReadNext ln file k -> do
            cfg    <- use config
            curDir <- use dir
            let ipaths = includePaths cfg
            mFound <- searchForNextInclude vfs curDir ipaths file
            readAux (file:files) ln file k mFound
          WriteOutput output k -> sink output >> runHppT k >>= go files

        readAux _files ln file _ Nothing =
          Left . (, IncludeDoesNotExist ln (stripAngleBrackets file))
               . curFileName <$> use config
        readAux files _ln _file k (Just file') =
          hppReadLines vfs file' >>= runHppT . k >>= go files
