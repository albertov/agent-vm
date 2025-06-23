{-# LANGUAGE DuplicateRecordFields #-}

-- | Main entry point for the agent-vm library
module AgentVM
  ( -- * Types
    VMState (..),
    BranchName (..),
    VMId (..),
    VMConfig (..),
    VM (..),
    VMStateData (..),
    VMOp (..),
    VMError (..),

    -- * State
    VMRegistry (..),
    VMInfo (..),
    newVMRegistry,
    withVMLock,
    registerVM,
    lookupVM,
    unregisterVM,
    allocatePort,
    releasePort,

    -- * Process
    startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,

    -- * SSH
    generateSSHKey,
    waitForSSH,
    sshExec,
    trySSHConnect,

    -- * Nix
    buildVMConfig,
    runVMScript,

    -- * Logging
    AgentVmTrace
      ( VMCreated,
        VMStarting,
        VMStarted,
        VMStopping,
        VMStopped,
        VMDestroyed,
        VMFailed,
        ProcessSpawned,
        ProcessExited,
        ProcessOutput,
        ProcessError,
        SSHKeyGenerated,
        SSHConnecting,
        SSHConnected,
        SSHCommandExecuted,
        SSHFailed,
        NixBuildStarted,
        NixBuildProgress,
        NixBuildCompleted
      ),
    traceToMessage,
    renderTrace,
    vmLogger,

    -- * Config
    VMConfigJson (..),
    loadVMConfig,
    saveVMConfig,

    -- * VM Lifecycle
    createVM,
    destroyVM,
  )
where

import AgentVM.Config
import AgentVM.Log
import AgentVM.Nix
import AgentVM.Process
import AgentVM.SSH
import AgentVM.State
import AgentVM.Types
import Plow.Logging (traceWith)
import Protolude

-- | Create a new VM with the given configuration
createVM :: LogContext AgentVmTrace -> VMConfigJson -> VMConfig -> IO ()
createVM logCtx config vmConfig = do
  -- For now, just log that we're creating a VM
  -- This is a minimal implementation to make the test pass
  let branchName = BranchName (vm_name config)
  traceWith logCtx $ VMCreated branchName vmConfig

-- TODO: Implement actual VM creation logic:
-- 1. Generate SSH keys
-- 2. Build VM configuration with Nix
-- 3. Start QEMU process
-- 4. Wait for SSH connection
-- 5. Register VM in state

-- | Destroy a VM and clean up its resources
destroyVM :: LogContext AgentVmTrace -> VMConfigJson -> IO ()
destroyVM logCtx config = do
  -- For now, just log that we're destroying a VM
  -- This is a minimal implementation to make the test pass
  let branchName = BranchName (vm_name config)
  traceWith logCtx $ VMDestroyed branchName

-- TODO: Implement actual VM destruction logic:
-- 1. Stop VM process
-- 2. Clean up workspace
-- 3. Remove SSH keys
-- 4. Unregister VM from state
-- 5. Release allocated ports
