{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | VM monad instance for running VM operations
module AgentVM.Monad
  ( VMT (..),
    runVMT,
  )
where

import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log (AgentVmTrace, MonadTrace (..))
import AgentVM.Types (VMConfig, VMError (..), VMHandle, VMState)
import Control.Monad.Reader (ReaderT (..))
import Data.Generics.Product (HasType)
import Plow.Logging (IOTracer, traceWith)
import Protolude
import UnliftIO (MonadUnliftIO)

-- | VM monad transformer
newtype VMT m a = VMT {unVMT :: ReaderT AgentVmEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AgentVmEnv, MonadUnliftIO)

-- | MonadTrace instance for VMT
instance (MonadIO m) => MonadTrace AgentVmTrace (VMT m) where
  trace msg = do
    env <- ask
    liftIO $ traceWith (tracer env) msg

-- | Run a VMT computation
runVMT :: AgentVmEnv -> VMT m a -> m a
runVMT env (VMT m) = runReaderT m env
