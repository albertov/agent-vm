-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
  )
where

import AgentVM.Log (AgentVmTrace, LogAction)
import Protolude (Bool, FilePath, IO, Int, Maybe, notImplemented)
import System.Process.Typed (ExitCode, Process)

-- | Start a VM process
startVMProcess :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
startVMProcess = notImplemented

-- | Stop a VM process gracefully
stopVMProcess :: LogAction IO AgentVmTrace -> Process () () () -> IO ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: Process () () () -> IO Bool
checkVMProcess = notImplemented

-- | Wait for a process with timeout
waitForProcess :: Int -> Process () () () -> IO (Maybe ExitCode)
waitForProcess = notImplemented
