{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig,
    runVMScript,
  )
where

import AgentVM.Log (AgentVmTrace (NixBuildCompleted, NixBuildFailed, NixBuildStarted, ProcessSpawned), (<&))
import AgentVM.Types (BranchName, VMError (NixBuildFailed), unBranchName)
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import Data.Generics.Product (HasType, the)
import qualified Data.Text as T
import Lens.Micro.Mtl (view)
import Plow.Logging (IOTracer (IOTracer), traceWith)
import Protolude
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), Process, byteStringOutput, closed, getStderr, getStdout, nullStream, proc, setStderr, setStdin, setStdout, startProcess, waitExitCode, withProcessWait)

-- | Build VM configuration using Nix
buildVMConfig ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  BranchName ->
  FilePath ->
  m (Either VMError FilePath)
buildVMConfig branchName workspace = notImplemented

-- | Run Nix-generated VM script
runVMScript ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  FilePath ->
  m (Process () () ())
runVMScript scriptPath = notImplemented
