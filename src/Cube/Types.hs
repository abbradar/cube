-- | Common engine types.

module Cube.Types where

import Control.Monad.Fix
import Control.Monad.Logger
import Control.Monad.IO.Class
import Control.Monad.Catch
import Control.Monad.Ref

type MonadCube m = (MonadMask m, MonadFix m, MonadFail m, MonadLoggerIO m, MonadIO m, MonadRef m, Ref m ~ Ref IO)
