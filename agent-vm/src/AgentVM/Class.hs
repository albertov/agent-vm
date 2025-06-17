{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | VM typeclass for abstracting VM operations
module AgentVM.Class
  ( MonadVM (..),
  )
where

import AgentVM.Types (VMConfig, VMError, VMHandle, VMState)
import Protolude

-- | Typeclass for VM operations
class (Monad m) => MonadVM m where
  -- | Create a new VM with the given configuration
  create :: VMConfig -> m (Either VMError VMHandle)

  -- | Destroy a VM and clean up its resources
  destroy :: VMHandle -> m (Either VMError ())

  -- | Start a stopped VM
  start :: VMHandle -> m (Either VMError ())

  -- | Stop a running VM
  stop :: VMHandle -> m (Either VMError ())

  -- | Get the current status of a VM
  status :: VMHandle -> m (Either VMError VMState)
