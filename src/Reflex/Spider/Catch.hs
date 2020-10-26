-- | MonadBase/MonadBaseControl instances for SpiderHost.

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Reflex.Spider.Catch where

import Control.Monad.Catch
import Reflex.Spider.Internal

deriving newtype instance MonadThrow (SpiderHost x)
deriving newtype instance MonadCatch (SpiderHost x)
deriving newtype instance MonadMask (SpiderHost x)

deriving newtype instance MonadFail (EventM x)
deriving newtype instance MonadThrow (EventM x)
deriving newtype instance MonadCatch (EventM x)
deriving newtype instance MonadMask (EventM x)

deriving newtype instance MonadFail (SpiderHostFrame x)
deriving newtype instance MonadThrow (SpiderHostFrame x)
deriving newtype instance MonadCatch (SpiderHostFrame x)
deriving newtype instance MonadMask (SpiderHostFrame x)
