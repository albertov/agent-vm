{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
    ProcessState (..),
    VMProcess (..),
    startLoggedProcess,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), MonadTrace (..))
import Protolude hiding (trace)
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
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  FilePath ->
  [Text] ->
  m (Process () () ())
startLoggedProcess scriptPath args = do
  -- Trace the process spawn event
  trace $ ProcessSpawned (toS scriptPath) args

  -- Start the process
  let processConfig = proc scriptPath (map toS args)
  liftIO $ startProcess processConfig

-- | Start a VM process
startVMProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  FilePath ->
  m VMProcess
startVMProcess scriptPath = do
  -- Use startLoggedProcess to start the process with logging
  process <- startLoggedProcess scriptPath []
  return $ VMProcess process

-- | Stop a VM process gracefully
stopVMProcess ::
  ( MonadTrace AgentVmTrace m,
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
