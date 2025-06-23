{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
    ProcessState (..),
    startLoggedProcess,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), (<&))
import Data.Generics.Product (HasType, the)
import Lens.Micro.Mtl (view)
import Plow.Logging (IOTracer (IOTracer), traceWith)
import Protolude
import System.Process.Typed (ExitCode (..), Process, getExitCode, proc, startProcess)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

newtype VMProcess = VMProcess {unVMProcess :: Process () () ()}

-- TODO: This should be capturing stderr and stdout of the process and tracing
-- them with the logger line by line for better debugging
startLoggedProcess ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  FilePath ->
  [Text] ->
  m (Process () () ())
startLoggedProcess scriptPath args = notImplemented

-- | Start a VM process
startVMProcess ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  FilePath ->
  m VMProcess
startVMProcess scriptPath = notImplemented

-- | Stop a VM process gracefully
stopVMProcess ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  VMProcess ->
  m ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: VMProcess -> IO ProcessState
checkVMProcess process = do
  exitCode <- getExitCode (unVMProcess process)
  return $ maybe ProcessRunning ProcessExited exitCode

-- | Wait for a process with timeout
waitForProcess :: Int -> Process a b c -> IO (Maybe ExitCode)
waitForProcess = notImplemented
