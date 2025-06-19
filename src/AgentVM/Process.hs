{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
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
  )
where

import AgentVM.Log (AgentVmTrace (ProcessError, ProcessOutput, ProcessSpawned), MonadTrace (..))
import Control.Concurrent.Thread.Delay (delay)
import Control.Concurrent.Timeout (timeout)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as TE
import Protolude
  ( Applicative,
    Either (..),
    Eq,
    FilePath,
    Functor,
    IO,
    IOException,
    Int,
    Integer,
    Maybe (..),
    Monad,
    MonadIO,
    MonadReader,
    Show,
    Text,
    fromIntegral,
    fromMaybe,
    isJust,
    liftIO,
    map,
    mapM_,
    maybe,
    pure,
    putStrLn,
    toS,
    ($),
    (&),
    (<$),
    (<*),
    (>>=),
  )
import System.Posix.Signals (sigKILL, signalProcess)
import System.Process.Typed
  ( ExitCode (..),
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
    bracket,
    catchAny,
    hSetBuffering,
    try,
  )
import UnliftIO.Async (async, cancel, wait, withAsync)

-- | State of a VM process
data ProcessState
  = ProcessRunning
  | ProcessExited ExitCode
  deriving (Eq, Show)

data VMProcess = VMProcess
  { getProcess :: Process () Handle Handle,
    waitIOThreads :: IO ()
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
