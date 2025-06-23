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

import AgentVM.Log (AgentVmTrace (ProcessError, ProcessOutput, ProcessSpawned), MonadTrace (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Protolude hiding (async, atomically, trace)
import System.IO (Handle)
import System.Process.Typed (ExitCode (..), Process, createPipe, getExitCode, getStderr, getStdout, proc, setStderr, setStdout, startProcess)
import UnliftIO (MonadUnliftIO, catchAny)
import UnliftIO.Async (async)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

newtype VMProcess = VMProcess {unVMProcess :: Process () Handle Handle}

startLoggedProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  m (Process () Handle Handle)
startLoggedProcess scriptPath args = do
  -- Trace the process spawn event
  trace $ ProcessSpawned (toS scriptPath) args

  -- Start the process with captured stdout and stderr
  let processConfig =
        proc scriptPath (map toS args)
          & setStdout createPipe
          & setStderr createPipe

  process <- liftIO $ startProcess processConfig

  -- Spawn async tasks to read and trace output
  let processName = toS scriptPath :: Text
  _ <- async $ traceHandleOutput processName ProcessOutput (getStdout process)
  _ <- async $ traceHandleOutput processName ProcessError (getStderr process)

  return process
  where
    -- Helper to trace output from a handle line by line
    traceHandleOutput :: (MonadTrace AgentVmTrace m, MonadUnliftIO m) => Text -> (Text -> Text -> AgentVmTrace) -> Handle -> m ()
    traceHandleOutput name constructor handle = do
      let loop = do
            line <- liftIO $ BS.hGetLine handle
            unless (BS.null line) $ do
              let lineText = TE.decodeUtf8Lenient line
              trace (constructor name lineText)
            loop
      loop `catchAny` (\_ -> return ())

-- | Start a VM process
startVMProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
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
