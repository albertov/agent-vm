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
    Severity (..),
    traceToMessage,
    traceSeverity,
    renderTrace,
    vmLogger,

    -- * Config
    VMConfigJson (..),
    loadVMConfig,
    saveVMConfig,
  )
where

import AgentVM.Config
import AgentVM.Log
import AgentVM.Nix
import AgentVM.Process
import AgentVM.SSH
import AgentVM.State
import AgentVM.Types
import Protolude ()
