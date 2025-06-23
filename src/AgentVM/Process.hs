{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Process management for VMs
module AgentVM.Process
  ( startVMProcess,
    stopVMProcess,
    withVMProcess,
    checkVMProcess,
    waitForProcess,
    ProcessState (..),
    VMProcess (..),
    VMProcessWithCapture (..),
    startLoggedProcess,
    startLoggedProcessWithCapture,

    -- * Functions with captured output
    waitForProcessCaptured,
    stopVMProcessCaptured,
    withVMProcessCaptured,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessError, ProcessOutput, ProcessSpawned), MonadTrace (trace))
import Control.Concurrent.Timeout (timeout)
import qualified Data.ByteString.Char8 as BS
import Data.List (reverse)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Protolude
  ( Either (Left, Right),
    Eq,
    FilePath,
    IO,
    IOException,
    Integer,
    Maybe (Just, Nothing),
    MonadIO,
    Show,
    Text,
    fromMaybe,
    isJust,
    liftIO,
    map,
    mapM_,
    maybe,
    pure,
    toS,
    ($),
    (&),
    (>>=),
  )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process.Typed
  ( ExitCode,
    Process,
    createPipe,
    getExitCode,
    getPid,
    getStderr,
    getStdout,
    nullStream,
    proc,
    setStderr,
    setStdin,
    setStdout,
    startProcess,
    stopProcess,
    waitExitCode,
  )
import UnliftIO
  ( BufferMode (LineBuffering),
    Handle,
    MonadUnliftIO,
    SomeException,
    bracket,
    catchAny,
    hSetBuffering,
    throwIO,
    try,
  )
import UnliftIO.Async (async, wait)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

data VMProcess = VMProcess
  { getProcess :: Process () Handle Handle,
    waitIOThreads :: IO ()
  }

-- | VM process with output capture
data VMProcessWithCapture = VMProcessWithCapture
  { getProcessCapture :: Process () Handle Handle,
    waitIOThreadsCapture :: IO (),
    capturedStdout :: IORef [Text],
    capturedStderr :: IORef [Text]
  }

startLoggedProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  m (Process () Handle Handle, IO ())
startLoggedProcess scriptPath args = do
  -- Trace the process spawn event
  trace $ ProcessSpawned (toS scriptPath) args

  -- Start the process with captured stdout and stderr
  let processConfig =
        proc scriptPath (map toS args)
          & setStdout createPipe
          & setStderr createPipe
          & setStdin nullStream

  process <- liftIO $ startProcess processConfig

  -- Get handles and set buffering BEFORE starting async readers
  let stdoutHandle = getStdout process
      stderrHandle = getStderr process

  -- Set line buffering on handles to ensure we read line by line
  liftIO $ do
    catchAny (hSetBuffering stdoutHandle LineBuffering) (\_ -> pure ())
    catchAny (hSetBuffering stderrHandle LineBuffering) (\_ -> pure ())

  -- Spawn async tasks to read and trace output
  let processName = toS scriptPath :: Text
  tOut <- async $ traceHandleOutput processName ProcessOutput stdoutHandle
  tErr <- async $ traceHandleOutput processName ProcessError stderrHandle

  pure (process, mapM_ wait [tOut, tErr])
  where
    -- Helper to trace output from a handle line by line
    traceHandleOutput :: (MonadTrace AgentVmTrace m, MonadUnliftIO m) => Text -> (Text -> Text -> AgentVmTrace) -> Handle -> m ()
    traceHandleOutput name constructor handle = do
      let loop = do
            eitherLine <- liftIO $ try $ BS.hGetLine handle
            case eitherLine of
              Left (_ :: IOException) -> pure () -- EOF or error
              Right line -> do
                -- Always trace the line, even if it's empty
                let lineText = TE.decodeUtf8Lenient line
                trace (constructor name lineText)
                loop
      loop

-- | Start a VM process
startVMProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  m VMProcess
startVMProcess scriptPath args = do
  -- Use startLoggedProcess to start the process with logging
  (getProcess, waitIOThreads) <- startLoggedProcess scriptPath args
  pure $ VMProcess {getProcess, waitIOThreads}

-- | Start a process with logging and output capture
startLoggedProcessWithCapture ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  m VMProcessWithCapture
startLoggedProcessWithCapture scriptPath args = do
  -- Trace the process spawn event
  trace $ ProcessSpawned (toS scriptPath) args

  -- Initialize capture refs
  stdoutCapture <- newIORef []
  stderrCapture <- newIORef []

  -- Start the process with captured stdout and stderr
  let processConfig =
        proc scriptPath (map toS args)
          & setStdout createPipe
          & setStderr createPipe
          & setStdin nullStream

  process <- liftIO $ startProcess processConfig

  -- Get handles and set buffering BEFORE starting async readers
  let stdoutHandle = getStdout process
      stderrHandle = getStderr process

  -- Set line buffering on handles to ensure we read line by line
  liftIO $ do
    catchAny (hSetBuffering stdoutHandle LineBuffering) (\_ -> pure ())
    catchAny (hSetBuffering stderrHandle LineBuffering) (\_ -> pure ())

  -- Spawn async tasks to read and trace output
  let processName = toS scriptPath :: Text
  tOut <- async $ traceAndCaptureOutput processName ProcessOutput stdoutHandle stdoutCapture
  tErr <- async $ traceAndCaptureOutput processName ProcessError stderrHandle stderrCapture

  pure $
    VMProcessWithCapture
      { getProcessCapture = process,
        waitIOThreadsCapture = mapM_ wait [tOut, tErr],
        capturedStdout = stdoutCapture,
        capturedStderr = stderrCapture
      }
  where
    -- Helper to trace and capture output from a handle line by line
    traceAndCaptureOutput ::
      (MonadTrace AgentVmTrace m, MonadUnliftIO m) =>
      Text ->
      (Text -> Text -> AgentVmTrace) ->
      Handle ->
      IORef [Text] ->
      m ()
    traceAndCaptureOutput name constructor handle captureRef = do
      let loop = do
            eitherLine <- liftIO $ try $ BS.hGetLine handle
            case eitherLine of
              Left (_ :: IOException) -> pure () -- EOF or error
              Right line -> do
                -- Always trace the line, even if it's empty
                let lineText = TE.decodeUtf8Lenient line
                trace (constructor name lineText)
                -- Capture the line
                liftIO $ atomicModifyIORef' captureRef (\xs -> (lineText : xs, ()))
                loop
      loop

type Timeout = Integer

-- | Stop a VM process gracefully
stopVMProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  Maybe Timeout ->
  VMProcess ->
  m ExitCode
stopVMProcess mTimeout vmProcess = do
  -- Stop the process
  liftIO $ stopProcess process
  loop
  where
    process = getProcess vmProcess
    loop =
      waitForProcess (fromMaybe 1000 mTimeout) vmProcess >>= \case
        Just ecode -> pure ecode
        Nothing | isJust mTimeout -> liftIO $ do
          getPid process >>= mapM_ (signalProcess sigKILL)
          waitExitCode process
        Nothing -> loop

-- | Stop a VM process gracefully and capture output
stopVMProcessCaptured ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  Maybe Timeout ->
  VMProcessWithCapture ->
  m (ExitCode, Text, Text)
stopVMProcessCaptured mTimeout vmProcess = do
  -- Stop the process
  liftIO $ stopProcess process
  -- Wait and get the captured output
  maybeResult <- waitForProcessCaptured (fromMaybe 1000 mTimeout) vmProcess
  case maybeResult of
    Just result -> pure result
    Nothing | isJust mTimeout -> liftIO $ do
      -- Force kill if timeout exceeded
      getPid process >>= mapM_ (signalProcess sigKILL)
      exitCode <- waitExitCode process
      -- Wait for IO threads and get output
      waitIOThreadsCapture vmProcess
      stdout <- readIORef (capturedStdout vmProcess)
      stderr <- readIORef (capturedStderr vmProcess)
      let stdoutText = T.intercalate "\n" (reverse stdout)
      let stderrText = T.intercalate "\n" (reverse stderr)
      pure (exitCode, stdoutText, stderrText)
    Nothing -> stopVMProcessCaptured mTimeout vmProcess -- Retry if no timeout set
  where
    process = getProcessCapture vmProcess

-- | Start a VM process and ensures that it is killed when
-- then continuation exits
withVMProcess ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  Maybe Timeout ->
  (VMProcess -> m a) ->
  m a
withVMProcess scriptPath args mTimeout =
  bracket
    (startVMProcess scriptPath args)
    (stopVMProcess mTimeout)

-- | Start a VM process with output capture and ensures that it is killed when
-- the continuation exits
withVMProcessCaptured ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  Maybe Timeout ->
  (VMProcessWithCapture -> m a) ->
  m (a, Text, Text)
withVMProcessCaptured scriptPath args mTimeout action = do
  -- Start the process with capture
  vmProcess <- startLoggedProcessWithCapture scriptPath args

  -- Run the action and handle cleanup
  result <- try $ action vmProcess

  -- Stop the process and get captured output
  (_, stdout, stderr) <- stopVMProcessCaptured mTimeout vmProcess

  -- Re-throw any exception from the action after cleanup
  case result of
    Left (e :: SomeException) -> throwIO e
    Right a -> pure (a, stdout, stderr)

-- | Check if VM process is still running
checkVMProcess :: (MonadIO m) => VMProcess -> m ProcessState
checkVMProcess process = liftIO $ do
  exitCode <- getExitCode (getProcess process)
  pure $ maybe ProcessRunning ProcessExited exitCode

-- | Wait for a process with timeout
waitForProcess :: (MonadIO m) => Integer -> VMProcess -> m (Maybe ExitCode)
waitForProcess timeoutMicros vmProcess = do
  -- Convert microseconds to Integer for unbounded-delays
  liftIO $ timeout timeoutMicros $ do
    exitCode <- waitExitCode (getProcess vmProcess)
    -- IMPORTANT: Wait for IO threads to complete to ensure all output is captured
    waitIOThreads vmProcess
    pure exitCode

-- | Wait for a process with timeout and capture output
waitForProcessCaptured :: (MonadIO m) => Integer -> VMProcessWithCapture -> m (Maybe (ExitCode, Text, Text))
waitForProcessCaptured timeoutMicros vmProcess = do
  liftIO $ timeout timeoutMicros $ do
    exitCode <- waitExitCode (getProcessCapture vmProcess)
    -- IMPORTANT: Wait for IO threads to complete to ensure all output is captured
    waitIOThreadsCapture vmProcess

    -- Read captured output
    stdout <- readIORef (capturedStdout vmProcess)
    stderr <- readIORef (capturedStderr vmProcess)

    -- Reverse the lists since we captured in reverse order
    -- and join with newlines
    let stdoutText = T.intercalate "\n" (reverse stdout)
    let stderrText = T.intercalate "\n" (reverse stderr)

    pure (exitCode, stdoutText, stderrText)
