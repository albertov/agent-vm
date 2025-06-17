-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), (<&))
import qualified Data.Text as T
import Plow.Logging (IOTracer (IOTracer))
import Protolude
import System.Process.Typed (ExitCode, Process, getExitCode, proc, startProcess)

-- | Start a VM process
startVMProcess :: IOTracer AgentVmTrace -> FilePath -> IO (Process () () ())
startVMProcess (IOTracer logger) scriptPath = do
  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess (proc scriptPath [])

-- | Stop a VM process gracefully
stopVMProcess :: IOTracer AgentVmTrace -> Process () () () -> IO ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: Process () () () -> IO Bool
checkVMProcess process = do
  exitCode <- getExitCode process
  return $ isNothing exitCode

-- | Wait for a process with timeout
waitForProcess :: Int -> Process () () () -> IO (Maybe ExitCode)
waitForProcess = notImplemented
