-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
    ProcessState (..),
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), (<&))
import qualified Data.Text as T
import Plow.Logging (IOTracer (IOTracer))
import Protolude
import System.Process.Typed (ExitCode (..), Process, getExitCode, proc, startProcess)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

-- | Start a VM process
-- TODO: This should be capturing stderr and stdout of the process and tracing
-- them with the logger line by line for better debugging
startVMProcess :: IOTracer AgentVmTrace -> FilePath -> IO (Process () () ())
startVMProcess (IOTracer logger) scriptPath = do
  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess (proc scriptPath [])

-- | Stop a VM process gracefully
stopVMProcess :: IOTracer AgentVmTrace -> Process () () () -> IO ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: Process () () () -> IO ProcessState
checkVMProcess process = do
  exitCode <- getExitCode process
  return $ maybe ProcessRunning ProcessExited exitCode

-- | Wait for a process with timeout
waitForProcess :: Int -> Process () () () -> IO (Maybe ExitCode)
waitForProcess = notImplemented
