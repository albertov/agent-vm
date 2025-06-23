{-# LANGUAGE FlexibleContexts #-}
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
    startLoggedProcess,
    startLoggedProcessWithCapture,

    -- * Functions with captured output
    waitForProcessCaptured,
    stopVMProcessCaptured,
    withVMProcessCaptured,
  )
where

import AgentVM.Log (AgentVmTrace (ProcessError, ProcessExited, ProcessGracefulStop, ProcessIOWaiting, ProcessOutput, ProcessSigKilled, ProcessSpawned, ProcessStopped, ProcessTimeout, ProcessWaitingForExit), MonadTrace (trace))
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
    const,
    fmap,
    fromIntegral,
    fromMaybe,
    isJust,
    liftIO,
    map,
    mapM_,
    maybe,
    not,
    pure,
    show,
    toS,
    void,
    ($),
    (&),
    (.),
    (<>),
    (>>=),
  )
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process.Typed
  ( ExitCode (ExitFailure, ExitSuccess),
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
    bracketOnError,
    catchAny,
    hSetBuffering,
    handleAny,
    throwIO,
    try,
  )
import UnliftIO.Async (async, wait)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessNotRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

data VMProcess = VMProcess
  { getProcess :: Process () Handle Handle,
    waitIOThreads :: IO (),
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
  -- Initialize empty capture refs for basic VMProcess
  stdoutCapture <- newIORef []
  stderrCapture <- newIORef []

  -- Use startLoggedProcess to start the process with logging
  (getProcess, waitIOThreads) <- startLoggedProcess scriptPath args
  pure $ VMProcess {getProcess, waitIOThreads, capturedStdout = stdoutCapture, capturedStderr = stderrCapture}

-- | Start a process with logging and output capture
startLoggedProcessWithCapture ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  m VMProcess
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
    VMProcess
      { getProcess = process,
        waitIOThreads = mapM_ wait [tOut, tErr],
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
stopVMProcess mTimeout =
  fmap (\(a, _, _) -> a) . stopVMProcessCaptured mTimeout

-- | Stop a VM process gracefully and capture output
stopVMProcessCaptured ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  Maybe Timeout ->
  VMProcess ->
  m (ExitCode, Text, Text)
stopVMProcessCaptured mTimeout vmProcess = do
  let process = getProcess vmProcess
      timeoutValue = fromMaybe 1000 mTimeout

  -- Get process name for tracing
  processName <- liftIO $ do
    maybePid <- getPid process
    pure $ case maybePid of
      Nothing -> "unknown-process"
      Just pid -> "pid-" <> T.pack (show pid)

  -- Trace the graceful stop attempt
  trace $ ProcessGracefulStop processName timeoutValue

  -- Stop the process
  liftIO $ timeout timeoutValue $ stopProcess process
  trace $ ProcessStopped processName
  -- Wait and get the captured output
  maybeResult <- waitForProcessCaptured timeoutValue vmProcess
  case maybeResult of
    Just result -> do
      let (exitCode, _, _) = result
      trace $ AgentVM.Log.ProcessExited processName (case exitCode of ExitSuccess -> 0; ExitFailure c -> c)
      pure result
    Nothing | isJust mTimeout -> do
      -- Force kill if timeout exceeded
      trace $ ProcessTimeout processName timeoutValue
      liftIO $ do
        getPid process
          >>= mapM_
            ( \pid -> do
                signalProcess sigKILL pid
            )
      trace $ ProcessSigKilled processName (fromIntegral sigKILL)
      exitCode <- liftIO $ waitExitCode process
      -- Wait for IO threads and get output
      trace $ ProcessIOWaiting processName
      liftIO $ do
        waitIOThreads vmProcess
        stdout <- readIORef (capturedStdout vmProcess)
        stderr <- readIORef (capturedStderr vmProcess)
        let stdoutText = T.intercalate "\n" (reverse stdout)
        let stderrText = T.intercalate "\n" (reverse stderr)
        pure (exitCode, stdoutText, stderrText)
    Nothing -> stopVMProcessCaptured mTimeout vmProcess -- Retry if no timeout set

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
    (handleAny (const (pure ())) . void . stopVMProcess mTimeout)

-- | Start a VM process with output capture and ensures that it is killed when
-- the continuation exits
withVMProcessCaptured ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  FilePath ->
  [Text] ->
  Maybe Timeout ->
  (VMProcess -> m a) ->
  m (a, Text, Text)
withVMProcessCaptured scriptPath args mTimeout action =
  -- Start the process with capture
  bracketOnError
    (startLoggedProcessWithCapture scriptPath args)
    (stopVMProcess mTimeout)
    $ \vmProcess -> do
      a <- action vmProcess
      -- Stop the process and get captured output
      (_, stdout, stderr) <- stopVMProcessCaptured mTimeout vmProcess
      pure (a, stdout, stderr)

-- | Check if VM process is still running
checkVMProcess :: (MonadTrace AgentVmTrace m, MonadIO m) => VMProcess -> m ProcessState
checkVMProcess process = do
  -- Get process name for tracing
  let proc = getProcess process
  maybePid <- liftIO $ getPid proc
  case maybePid of
    Nothing -> pure ProcessNotRunning
    Just pid -> do
      let procPath = "/proc" </> show pid
      isRunning <- liftIO $ doesDirectoryExist procPath
      if not isRunning
        then pure ProcessNotRunning
        else do
          let processName = "pid-" <> T.pack (show pid)
          eExitCode <- liftIO $ try $ getExitCode proc
          case eExitCode of
            Right exitCode -> do
              let state = maybe ProcessRunning AgentVM.Process.ProcessExited exitCode
              -- Trace process state changes only when process has exited
              case state of
                AgentVM.Process.ProcessExited code -> trace $ AgentVM.Log.ProcessExited processName (case code of ExitSuccess -> 0; ExitFailure c -> c)
                ProcessRunning -> pure () -- Don't spam logs for running processes
              pure state
            Left (_ :: IOException) -> pure ProcessNotRunning

-- | Wait for a process with timeout
waitForProcess :: (MonadTrace AgentVmTrace m, MonadIO m) => Integer -> VMProcess -> m (Maybe ExitCode)
waitForProcess timeoutMicros =
  fmap (fmap (\(a, _, _) -> a)) . waitForProcessCaptured timeoutMicros

-- | Wait for a process with timeout and capture output
waitForProcessCaptured :: (MonadTrace AgentVmTrace m, MonadIO m) => Integer -> VMProcess -> m (Maybe (ExitCode, Text, Text))
waitForProcessCaptured timeoutMicros vmProcess = do
  -- Get process name for tracing
  let process = getProcess vmProcess
  processName <- liftIO $ do
    maybePid <- getPid process
    pure $ case maybePid of
      Nothing -> "unknown-process"
      Just pid -> "pid-" <> T.pack (show pid)

  trace $ ProcessWaitingForExit processName

  result <- liftIO $ timeout timeoutMicros $ do
    exitCode <- waitExitCode process
    -- IMPORTANT: Wait for IO threads to complete to ensure all output is captured
    waitIOThreads vmProcess

    -- Read captured output
    stdout <- readIORef (capturedStdout vmProcess)
    stderr <- readIORef (capturedStderr vmProcess)

    -- Reverse the lists since we captured in reverse order
    -- and join with newlines
    let stdoutText = T.intercalate "\n" (reverse stdout)
    let stderrText = T.intercalate "\n" (reverse stderr)

    pure (exitCode, stdoutText, stderrText)

  case result of
    Just (exitCode, stdout, stderr) -> do
      trace $ AgentVM.Log.ProcessExited processName (case exitCode of ExitSuccess -> 0; ExitFailure c -> c)
      trace $ ProcessIOWaiting processName
      pure (Just (exitCode, stdout, stderr))
    Nothing -> do
      trace $ ProcessTimeout processName timeoutMicros
      pure Nothing
