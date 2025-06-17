-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    checkVMProcess,
    waitForProcess,
    ProcessState (..),
    startLoggedProcess
  )
where

import AgentVM.Log (AgentVmTrace (ProcessSpawned), (<&))
import Plow.Logging (IOTracer (IOTracer))
import Protolude
import System.Process.Typed (ExitCode (..), Process, getExitCode, proc, startProcess)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

newtype VMProcess = VMProcess { unVMProcess :: Process () () () }

-- TODO: This should be capturing stderr and stdout of the process and tracing
-- them with the logger line by line for better debugging
startLoggedProcess :: IOTracer AgentVmTrace -> FilePath -> [Text] -> IO (Process () () ())
startLoggedProcess (IOTracer logger) scriptPath args = do
  logger <& ProcessSpawned (toS scriptPath) args
  startProcess (proc scriptPath (map toS args))

-- | Start a VM process
startVMProcess :: IOTracer AgentVmTrace -> FilePath -> IO VMProcess
startVMProcess logger scriptPath = 
  VMProcess <$> startLoggedProcess logger scriptPath []

-- | Stop a VM process gracefully
stopVMProcess :: IOTracer AgentVmTrace -> VMProcess -> IO ()
stopVMProcess = notImplemented

-- | Check if VM process is still running
checkVMProcess :: VMProcess -> IO ProcessState
checkVMProcess process = do
  exitCode <- getExitCode (unVMProcess process)
  return $ maybe ProcessRunning ProcessExited exitCode

-- | Wait for a process with timeout
waitForProcess :: Int -> Process a b c -> IO (Maybe ExitCode)
waitForProcess = notImplemented
