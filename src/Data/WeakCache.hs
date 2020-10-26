-- | Cache based on a hash map which holds weak references to its values.

{-# LANGUAGE StrictData #-}

module Data.WeakCache
  ( WeakCache
  , new
  , getOrCreate
  ) where

import Data.Tuple
import Control.Monad
import Data.IORef
import Control.Exception (throw)
import Control.Concurrent.MVar
import System.Mem.Weak
import Control.Monad.IO.Class
import Control.Monad.Catch
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HMS
import Data.Hashable

data WeakCache k a = WeakCache { cacheRef :: IORef (HashMap k (Int, MVar (Weak a)))
                               , cacheGenerationRef :: IORef Int
                               -- ^ Used to avoid removing replaced entries from finalizers.
                               }

new :: MonadIO m => m (WeakCache k a)
new = liftIO (WeakCache <$> newIORef HMS.empty <*> newIORef 0)

getOrCreate :: forall m k a. (Eq k, Hashable k, MonadIO m, MonadMask m) => k -> m a -> WeakCache k a -> m a
getOrCreate key create (WeakCache {..}) = do
  mret <-  liftIO $ HMS.lookup key <$> readIORef cacheRef
  case mret of
    Nothing -> beginSlowPath Nothing
    Just (curGen, mv) -> do
      maybeVal <- liftIO $ readMVar mv >>= deRefWeak
      case maybeVal of
        Nothing -> beginSlowPath $ Just curGen
        Just ret -> return ret

  where beginSlowPath :: Maybe Int -> m a
        beginSlowPath mPrevGen = do
          createLock <- liftIO newEmptyMVar
          myGen <- liftIO $ atomicModifyIORef' cacheGenerationRef (\gen -> (gen + 1, gen))
          slowPath createLock myGen mPrevGen

        slowPath :: MVar (Weak a) -> Int -> Maybe Int -> m a
        slowPath createLock myGen mPrevGen = do
          mask $ \restore -> do
            let alterKey jpair@(Just (gen, _)) | Just prevGen <- mPrevGen, gen == prevGen = (jpair, jpair)
                alterKey _ = (Nothing, Just (myGen, createLock))
            mret <- liftIO $ atomicModifyIORef' cacheRef (swap . HMS.alterF alterKey key)
            case mret of
              Just (curGen, mv) -> restore $ do
                maybeVal <- liftIO $ readMVar mv >>= deRefWeak
                case maybeVal of
                  Nothing -> slowPath createLock myGen $ Just curGen
                  Just !ret -> return ret
              Nothing -> do
                let finalizer :: IO ()
                    finalizer = do
                      let finalizeKey (Just (newGen, _)) | myGen == newGen = Nothing
                          finalizeKey val = val
                      modifyIORef' cacheRef $ HMS.alter finalizeKey key
                    createValue = do
                      !r <- create
                      weakR <- liftIO $ mkWeakPtr r (Just finalizer)
                      liftIO $ putMVar createLock weakR
                      return r
                    putError (e :: SomeException) = do
                      succeeded <- liftIO $ tryPutMVar createLock (throw e)
                      when succeeded $ liftIO $ modifyIORef' cacheRef (HMS.delete key)
                      throwM e

                restore createValue `catch` putError
