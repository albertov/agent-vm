{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | VM monad instance for running VM operations
module AgentVM.Monad
  ( VMT (..),
    runVMT,
  )
where

import AgentVM (createVM, destroyVM)
import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv)
import AgentVM.Log (AgentVmTrace)
import AgentVM.Types (VMConfig, VMError (..), VMHandle, VMState)
import Control.Monad.Reader (ReaderT (..))
import Data.Generics.Product (HasType)
import Plow.Logging (IOTracer)
import Protolude

-- | VM monad transformer
newtype VMT m a = VMT {unVMT :: ReaderT AgentVmEnv m a}
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader AgentVmEnv)

-- | Run a VMT computation
runVMT :: AgentVmEnv -> VMT m a -> m a
runVMT env (VMT m) = runReaderT m env

-- | Instance of MonadVM for VMT IO
instance MonadVM (VMT IO) where
  create config = do
    handle <- createVM config
    return (Right handle)

  destroy handle = do
    destroyVM handle
    return (Right ())

  -- These are not implemented yet
  start _ = return (Left (VMInvalidState "start not implemented"))
  stop _ = return (Left (VMInvalidState "stop not implemented"))
  status _ = return (Left (VMInvalidState "status not implemented"))
