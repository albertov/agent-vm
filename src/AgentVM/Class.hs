{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | VM typeclass for abstracting VM operations
module AgentVM.Class
  ( MonadVM (..),
  )
where

import AgentVM.Types (VMConfig, VMError, VMState)
import Protolude (Either, Monad)

-- | Typeclass for VM operations
class (Monad m) => MonadVM m where
  -- | Create a new VM with the given configuration
  create :: VMConfig -> m (Either VMError ())

  -- | Destroy a VM and clean up its resources
  destroy :: VMConfig -> m (Either VMError ())

  -- | Start a stopped VM
  start :: VMConfig -> m (Either VMError ())

  -- | Stop a running VM
  stop :: VMConfig -> m (Either VMError ())

  -- | Get the current status of a VM
  status :: VMConfig -> m (Either VMError VMState)

  -- | Connect to VM shell via serial console
  shell :: VMConfig -> m (Either VMError ())

  -- | Update VM configuration (regenerate start script and nix config without removing qcow2)
  update :: VMConfig -> m (Either VMError ())
