-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess
  , stopVMProcess
  , checkVMProcess
  , waitForProcess
  ) where

import AgentVM.Types
import AgentVM.Log
import System.Process.Typed
import Data.Text (Text)

-- | Start a VM process
startVMProcess :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
startVMProcess = error "startVMProcess has not been implemented yet"

-- | Stop a VM process gracefully
stopVMProcess :: LogAction IO AgentVmTrace -> Process () () () -> IO ()
stopVMProcess = error "stopVMProcess has not been implemented yet"

-- | Check if VM process is still running
checkVMProcess :: Process () () () -> IO Bool
checkVMProcess = error "checkVMProcess has not been implemented yet"

-- | Wait for a process with timeout
waitForProcess :: Int -> Process () () () -> IO (Maybe ExitCode)
waitForProcess = error "waitForProcess has not been implemented yet"
