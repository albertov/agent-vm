{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AgentVM.StreamingProcess
  ( Process (..),
    ProcessPty (..),
  )
where

import AgentVM.Interactive (InteractiveBackend (..))
import Control.Concurrent.STM (readTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import Control.Concurrent.Thread.Delay (delay)
import qualified Data.ByteString as BS
import Data.STM.RollingQueue (RollingQueue, write)
import qualified Data.STM.RollingQueue as RQ
import qualified Data.Text as T
import Protolude hiding (async, bracket, cancel, catch, finally, throwIO, try)
import System.IO (BufferMode (NoBuffering), hClose, hFlush, hSetBuffering)
import System.IO.Error (userError)
import System.Posix.IO (fdToHandle)
import System.Posix.Terminal (TerminalMode (..), getTerminalAttributes, openPseudoTerminal, setTerminalAttributes, withoutMode)
import qualified System.Posix.Terminal as Term
import System.Process (ProcessHandle, StdStream (..), createProcess, getProcessExitCode, proc, std_err, std_in, std_out, terminateProcess)
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (catch, throwIO, try)

-- | Backend type for process-based interactive sessions (using pipes)
data Process = Process FilePath [T.Text]

-- | Backend type for PTY-based interactive sessions
data ProcessPty = ProcessPty FilePath [T.Text]

-- | Shared cleanup data for both backends
data ProcessCleanup = ProcessCleanup ProcessHandle (Async ()) (Async ()) (Async ()) (Async ())

-- | InteractiveBackend instance for Process (pipes)
instance InteractiveBackend Process where
  data CleanupData Process = PipeCleanup ProcessCleanup

  startBackend (Process cmd args) readQueue writeQueue writeClosed = do
    result <-
      try $
        createProcess
          (proc cmd (map toS args))
            { std_in = CreatePipe,
              std_out = CreatePipe,
              std_err = CreatePipe
            }

    case result of
      Left (e :: SomeException) -> do
        -- Process creation failed, signal EOF
        atomically $ do
          write readQueue Nothing
          writeTVar writeClosed True
        throwIO e
      Right (Just stdinHandle, Just stdoutHandle, Just stderrHandle, ph) -> do
        -- Set no buffering for immediate byte-by-byte communication
        hSetBuffering stdinHandle NoBuffering
        hSetBuffering stdoutHandle NoBuffering
        hSetBuffering stderrHandle NoBuffering

        -- Start async threads for handling I/O
        cleanup <- startProcessThreads ph stdoutHandle stdinHandle readQueue writeQueue writeClosed
        pure $ PipeCleanup cleanup
      _ -> do
        -- Failed to create pipes properly
        atomically $ do
          write readQueue Nothing
          writeTVar writeClosed True
        throwIO $ userError "Failed to create process pipes"

  cleanupBackend (PipeCleanup cleanup) = cleanupProcess cleanup

-- | InteractiveBackend instance for ProcessPty
instance InteractiveBackend ProcessPty where
  data CleanupData ProcessPty = PtyCleanup ProcessCleanup

  startBackend (ProcessPty cmd args) readQueue writeQueue writeClosed = do
    -- Create a pseudo-terminal pair
    (masterFd, slaveFd) <- openPseudoTerminal

    -- Configure the PTY to disable echo and other terminal processing
    slaveAttrs <- getTerminalAttributes slaveFd
    let rawAttrs =
          slaveAttrs
            `withoutMode` EnableEcho -- Disable echo
            `withoutMode` ProcessInput -- Disable canonical mode
            `withoutMode` ProcessOutput -- Disable output processing
            `withoutMode` ExtendedFunctions
    setTerminalAttributes slaveFd rawAttrs Term.Immediately

    -- Convert file descriptors to handles
    masterHandle <- fdToHandle masterFd
    slaveHandle <- fdToHandle slaveFd

    -- Set no buffering for immediate byte-by-byte communication
    hSetBuffering masterHandle NoBuffering

    -- Create process using the slave as stdin/stdout/stderr
    result <-
      try $
        createProcess
          (proc cmd (map toS args))
            { std_in = UseHandle slaveHandle,
              std_out = UseHandle slaveHandle,
              std_err = UseHandle slaveHandle
            }

    case result of
      Left (e :: SomeException) -> do
        -- Process creation failed, signal EOF
        atomically $ do
          write readQueue Nothing
          writeTVar writeClosed True
        throwIO e
      Right (_, _, _, ph) -> do
        -- Close slave handle in parent (child has it now)
        hClose slaveHandle

        -- Start async threads for handling I/O
        cleanup <- startProcessThreads ph masterHandle masterHandle readQueue writeQueue writeClosed
        pure $ PtyCleanup cleanup

  cleanupBackend (PtyCleanup cleanup) = cleanupProcess cleanup

-- | Start the async threads for process I/O (shared implementation)
startProcessThreads :: ProcessHandle -> Handle -> Handle -> RollingQueue (Maybe BS.ByteString) -> RollingQueue BS.ByteString -> TVar Bool -> IO ProcessCleanup
startProcessThreads ph readHandle writeHandle readQueue writeQueue writeClosed = do
  readerAsync <- async $ readerThread readHandle readQueue
  dummyAsync <- async $ return () -- Placeholder for potential stderr reader
  writerAsync <- async $ writerThread writeHandle writeQueue writeClosed
  watcherAsync <- async $ processWatcher ph readQueue writeClosed
  pure $ ProcessCleanup ph readerAsync dummyAsync writerAsync watcherAsync

-- | Cleanup process resources (shared implementation)
cleanupProcess :: ProcessCleanup -> IO ()
cleanupProcess (ProcessCleanup ph readerAsync stderrAsync writerAsync watcherAsync) = do
  -- Cancel all async threads first
  cancel readerAsync
  cancel stderrAsync
  cancel writerAsync
  cancel watcherAsync
  -- Try to terminate the process but don't wait indefinitely
  terminateProcess ph `catch` \(_ :: IOException) -> return ()

-- | Thread that reads bytes from process stdout and puts them in the read queue
readerThread :: Handle -> RollingQueue (Maybe BS.ByteString) -> IO ()
readerThread hdl readQueue = do
  let loop = do
        -- Try to read available bytes (up to 4096 at a time)
        result <- try $ BS.hGetSome hdl 4096
        case result of
          Left (_ :: IOException) -> return () -- Handle closed or process ended
          Right chunk ->
            if BS.null chunk
              then return () -- EOF on this handle
              else do
                atomically $ write readQueue (Just chunk)
                loop

  -- Run the loop - processWatcher will signal EOF when process exits
  loop

-- | Thread that takes bytes from write queue and sends them to process stdin
writerThread :: Handle -> RollingQueue BS.ByteString -> TVar Bool -> IO ()
writerThread writeHandle writeQueue writeClosed = do
  let loop = do
        -- Use STM to atomically check if closed or read data
        result <- atomically $ do
          closed <- readTVar writeClosed
          if closed
            then return Nothing -- Signal to exit
            else do
              -- This will block until data is available
              (bytes, _discardCount) <- RQ.read writeQueue
              return (Just bytes)
        case result of
          Nothing -> return () -- Queue closed, exit
          Just bytes -> do
            BS.hPut writeHandle bytes
            hFlush writeHandle -- Ensure immediate delivery
            loop
  loop `catch` \(_ :: IOException) -> return () -- Process ended or handle closed

-- | Thread that monitors process liveness and closes queues when it exits
processWatcher :: ProcessHandle -> RollingQueue (Maybe BS.ByteString) -> TVar Bool -> IO ()
processWatcher ph readQueue writeClosed = do
  let checkLoop = do
        -- Check if process has exited (non-blocking)
        exitCode <- getProcessExitCode ph
        case exitCode of
          Nothing -> do
            -- Process still running, check again after a delay
            delay 100000 -- 100ms
            checkLoop
          Just _ -> do
            -- Process has exited
            delay 200000 -- 200ms delay to ensure readers finish and client reads data
            -- Signal EOF and close write queue
            atomically $ do
              write readQueue Nothing
              writeTVar writeClosed True
  checkLoop
