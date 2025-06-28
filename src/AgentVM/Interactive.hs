{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AgentVM.Interactive
  ( Interactive (..),
    InteractiveBackend (..),
    withInteractive,
    startInteractive,
    closeInteractive,
    readBytes,
    tryReadBytes,
    writeBytes,
    interactWith,
    parseEscapeKey,
  )
where

import Control.Concurrent.STM (newTVarIO, readTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import qualified Data.ByteString as BS
import Data.STM.RollingQueue (RollingQueue, newIO, write)
import qualified Data.STM.RollingQueue as RQ
import qualified Data.Text as T
import Protolude hiding (Handle, bracket, hPutStrLn, race_, throwIO)
import System.IO (Handle, hFlush, hPutStrLn)
import System.IO.Error (eofErrorType, mkIOError)
import System.Posix.IO (stdInput)
import System.Posix.Terminal (TerminalAttributes, TerminalMode (..), getTerminalAttributes, setTerminalAttributes, withoutMode)
import qualified System.Posix.Terminal as Term
import UnliftIO.Async (race_)
import UnliftIO.Exception (bracket)

-- | Generic interactive type for bidirectional byte streaming
data Interactive backend = Interactive
  { iReadQueue :: RollingQueue (Maybe BS.ByteString), -- Queue for reading output (Nothing = EOF)
    iWriteQueue :: RollingQueue BS.ByteString, -- Queue for writing input
    iWriteClosed :: TVar Bool, -- Track if write queue is closed
    iEofReached :: TVar Bool, -- Track if EOF has been reached
    iCleanup :: CleanupData backend -- Backend-specific cleanup data
  }

-- | Typeclass for backend-specific operations
class InteractiveBackend backend where
  -- | Associated cleanup data for this backend
  data CleanupData backend

  -- | Start the backend and return cleanup data
  startBackend :: backend -> RollingQueue (Maybe BS.ByteString) -> RollingQueue BS.ByteString -> TVar Bool -> IO (CleanupData backend)

  -- | Clean up backend resources
  cleanupBackend :: CleanupData backend -> IO ()

-- | Create and manage an interactive session with proper resource cleanup
withInteractive :: (InteractiveBackend backend) => Int -> backend -> (Interactive backend -> IO a) -> IO a
withInteractive queueSize backend =
  bracket
    (startInteractive queueSize backend)
    closeInteractive

-- | Start an interactive session for manual lifecycle management
startInteractive :: (InteractiveBackend backend) => Int -> backend -> IO (Interactive backend)
startInteractive queueSize backend = do
  -- Create bounded queues to prevent unbounded memory growth
  readQueue <- newIO queueSize
  writeQueue <- newIO queueSize
  writeClosed <- newTVarIO False
  eofReached <- newTVarIO False
  cleanupData <- startBackend backend readQueue writeQueue writeClosed
  return $ Interactive readQueue writeQueue writeClosed eofReached cleanupData

-- | Close an interactive session and clean up all resources
closeInteractive :: (InteractiveBackend backend) => Interactive backend -> IO ()
closeInteractive interactive = cleanupBackend (iCleanup interactive)

-- | Read bytes from the interactive session
-- Blocks until bytes are available or the session terminates
readBytes :: Interactive backend -> IO BS.ByteString
readBytes interactive = atomically $ do
  -- First check if we've already reached EOF
  eofAlready <- readTVar (iEofReached interactive)
  if eofAlready
    then throwSTM $ mkIOError eofErrorType "Interactive stream closed" Nothing Nothing
    else do
      (maybeBytes, _discardCount) <- RQ.read (iReadQueue interactive)
      case maybeBytes of
        Nothing -> do
          -- Mark EOF reached for future calls
          writeTVar (iEofReached interactive) True
          throwSTM $ mkIOError eofErrorType "Interactive stream closed" Nothing Nothing
        Just bytes -> return bytes

-- | Try to read bytes from the interactive session
-- Returns Nothing on EOF instead of throwing an exception
tryReadBytes :: Interactive backend -> IO (Maybe BS.ByteString)
tryReadBytes interactive = atomically $ do
  -- First check if we've already reached EOF
  eofAlready <- readTVar (iEofReached interactive)
  if eofAlready
    then return Nothing
    else do
      (maybeBytes, _discardCount) <- RQ.read (iReadQueue interactive)
      case maybeBytes of
        Nothing -> do
          -- Mark EOF reached for future calls
          writeTVar (iEofReached interactive) True
          return Nothing
        Just bytes -> return (Just bytes)

-- | Write bytes to the interactive session
-- Throws an exception if the session has terminated
writeBytes :: Interactive backend -> BS.ByteString -> IO ()
writeBytes interactive bytes = atomically $ do
  closed <- readTVar (iWriteClosed interactive)
  if closed
    then
      throwSTM $
        mkIOError eofErrorType "Interactive stream closed" Nothing Nothing
    else write (iWriteQueue interactive) bytes

-- | Set terminal to raw mode and return original attributes
setupRawMode :: IO TerminalAttributes
setupRawMode = do
  original <- getTerminalAttributes stdInput
  let rawAttrs =
        original
          `withoutMode` EnableEcho -- Disable echo
          `withoutMode` ProcessInput -- Disable canonical mode (line buffering)
          `withoutMode` ExtendedFunctions -- Disable extended input processing
          `withoutMode` KeyboardInterrupts -- Let the VM handle Ctrl-C
          `withoutMode` CheckParity -- Don't check parity
          `withoutMode` StripHighBit -- Don't strip high bit
  setTerminalAttributes stdInput rawAttrs Term.Immediately
  return original

-- | Restore original terminal attributes
restoreTerminalMode :: TerminalAttributes -> IO ()
restoreTerminalMode original = do
  setTerminalAttributes stdInput original Term.Immediately

-- | Parse escape key string into byte sequence
-- Supports formats like "Ctrl-A", "Ctrl-W", etc.
parseEscapeKey :: Text -> Word8
parseEscapeKey escapeKey = case T.toLower escapeKey of
  "ctrl-a" -> 1
  "ctrl-b" -> 2
  "ctrl-c" -> 3
  "ctrl-d" -> 4
  "ctrl-e" -> 5
  "ctrl-f" -> 6
  "ctrl-g" -> 7
  "ctrl-h" -> 8
  "ctrl-i" -> 9
  "ctrl-j" -> 10
  "ctrl-k" -> 11
  "ctrl-l" -> 12
  "ctrl-m" -> 13
  "ctrl-n" -> 14
  "ctrl-o" -> 15
  "ctrl-p" -> 16
  "ctrl-q" -> 17
  "ctrl-r" -> 18
  "ctrl-s" -> 19
  "ctrl-t" -> 20
  "ctrl-u" -> 21
  "ctrl-v" -> 22
  "ctrl-w" -> 23
  "ctrl-x" -> 24
  "ctrl-y" -> 25
  "ctrl-z" -> 26
  _ -> 23 -- Default to Ctrl-W (23)

-- | Interactive session with explicit handles for stdin, stdout, and stderr
interactWith :: (MonadIO m, InteractiveBackend backend) => Handle -> Handle -> Handle -> Text -> backend -> m ()
interactWith hStdin hStdout hStderr shellEscapeKey backend = do
  let escapeKeyByte = parseEscapeKey shellEscapeKey
  -- Connect to the VM serial socket with proper terminal raw mode
  liftIO $
    bracket
      setupRawMode
      restoreTerminalMode
      ( \_ -> withInteractive 1000 backend $ \socket -> do
          hPutStrLn hStdout ("\r\nConnected to VM serial console. Press " <> toS shellEscapeKey <> " then 'x' to exit.")
          hPutStrLn hStdout ("Note: Use " <> toS shellEscapeKey <> " x to exit cleanly. Ctrl+D will exit the shell but not the console.")
          hFlush hStdout
          -- Use race_ to ensure both threads exit when one finishes/fails
          race_
            (inputRelayWithEscape hStdin socket escapeKeyByte)
            (outputRelay hStdout socket)
      )
  where
    -- Input relay with escape sequence detection (configurable escape key + x to exit)
    inputRelayWithEscape hIn socket escapeKeyByte = do
      let loop escapeMode = do
            input <- BS.hGetSome hIn 1024 -- Read chunks (whatever is available)
            if BS.null input
              then return () -- EOF on stdin, exit gracefully
              else do
                -- Check for escape sequence: configured escape key followed by 'x'
                (shouldExit, nextEscapeMode) <- handleEscapeSequence escapeMode input socket hStderr escapeKeyByte
                unless shouldExit $ loop nextEscapeMode
      loop False

    -- Handle escape sequence detection and normal input
    -- Returns (shouldExit, nextEscapeMode)
    handleEscapeSequence escapeMode input socket hErr escapeKeyByte = do
      let inputList = BS.unpack input
      case (escapeMode, inputList) of
        -- Already in escape mode, check for 'x'
        (True, 120 : _) -> do
          -- 'x' = 120
          hPutStrLn hErr "\r\nExiting..."
          return (True, False)
        (True, _) -> do
          -- Not 'x', send the escape key and current input
          writeBytes socket (BS.pack [escapeKeyByte])
          writeBytes socket input
          return (False, False)
        -- Check for configured escape key
        (False, [keyByte]) | keyByte == escapeKeyByte -> return (False, True) -- Enter escape mode, don't send escape key yet
        (False, keyByte : rest) | keyByte == escapeKeyByte -> do
          -- Escape key followed by other chars
          case rest of
            120 : _ -> do
              -- 'x'
              hPutStrLn hErr "\r\nExiting..."
              return (True, False)
            _ -> do
              writeBytes socket input
              return (False, False)
        (False, _) -> do
          -- Normal input, send as-is
          writeBytes socket input
          return (False, False)

    -- Output relay: socket -> stdout (exits on socket EOF)
    outputRelay hOut socket = do
      let loop = do
            maybeOutput <- tryReadBytes socket
            case maybeOutput of
              Nothing -> return () -- EOF, exit gracefully
              Just output -> do
                BS.hPut hOut output
                hFlush hOut
                loop
      loop
