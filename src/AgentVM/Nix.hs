{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig,
    runVMScript,
  )
where

import AgentVM.Log (AgentVmTrace (NixBuildCompleted, NixBuildFailed, NixBuildStarted, ProcessSpawned), MonadTrace)
import AgentVM.Types (BranchName, VMError (NixBuildFailed), unBranchName)
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Protolude
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), Process, byteStringOutput, closed, getStderr, getStdout, nullStream, proc, setStderr, setStdin, setStdout, startProcess, waitExitCode, withProcessWait)

-- | Build VM configuration using Nix
buildVMConfig ::
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  BranchName ->
  FilePath ->
  m (Either VMError FilePath)
buildVMConfig branchName workspace = notImplemented

-- | Run Nix-generated VM script
runVMScript ::
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  FilePath ->
  m (Process () () ())
runVMScript scriptPath = notImplemented
