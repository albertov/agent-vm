-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), LogAction, (<&))
import qualified Data.Text as T
import Protolude
import System.Process.Typed (ExitCode, Process, getExitCode, proc, startProcess)

-- | Start a VM process
startVMProcess :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
startVMProcess logger scriptPath = do
  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess (proc scriptPath [])

-- | Stop a VM process gracefully
stopVMProcess :: LogAction IO AgentVmTrace -> Process () () () -> IO ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: Process () () () -> IO Bool
checkVMProcess process = do
  exitCode <- getExitCode process
  return $ isNothing exitCode

-- | Wait for a process with timeout
waitForProcess :: Int -> Process () () () -> IO (Maybe ExitCode)
waitForProcess = notImplemented
