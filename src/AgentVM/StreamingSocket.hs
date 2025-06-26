{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module AgentVM.StreamingSocket
  ( Socket (..),
  )
where

import AgentVM.Interactive (InteractiveBackend (..))
import Control.Concurrent.STM (readTVar, writeTVar)
import Control.Concurrent.STM.TVar (TVar)
import qualified Data.ByteString as BS
import Data.STM.RollingQueue (RollingQueue, tryRead, write)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, socket)
import qualified Network.Socket as NS
import Network.Socket.ByteString (recv, sendAll)
import Protolude hiding (async, bracket, cancel, catch, finally, throwIO)
import UnliftIO.Async (async, cancel)
import UnliftIO.Exception (catch, finally)

-- | Backend type for socket-based interactive sessions
newtype Socket = Socket FilePath

-- | InteractiveBackend instance for Socket
instance InteractiveBackend Socket where
  data CleanupData Socket = SocketCleanup NS.Socket (Async ()) (Async ())

  startBackend _queueSize (Socket socketPath) readQueue writeQueue writeClosed = do
    -- Create and connect to Unix domain socket
    sock <- socket AF_UNIX Stream 0
    connect sock (SockAddrUnix socketPath)

    -- Start async threads for handling I/O
    readerAsync <- async $ readerThread sock readQueue writeClosed
    writerAsync <- async $ writerThread sock writeQueue writeClosed

    pure $ SocketCleanup sock readerAsync writerAsync

  cleanupBackend (SocketCleanup sock readerAsync writerAsync) = do
    -- Cancel all async threads
    cancel readerAsync
    cancel writerAsync
    -- Shutdown socket gracefully before closing
    NS.shutdown sock NS.ShutdownBoth `catch` \(_ :: IOException) -> return ()
    -- Close the socket
    close sock

-- | Thread that reads bytes from socket and puts chunks in the read queue
readerThread :: NS.Socket -> RollingQueue (Maybe BS.ByteString) -> TVar Bool -> IO ()
readerThread sock readQueue writeClosed = do
  let loop = do
        bytes <- recv sock 4096 -- Read up to 4KB at a time
        if BS.null bytes
          then do
            -- EOF received, close write queue and signal end
            atomically $ do
              write readQueue Nothing
              writeTVar writeClosed True
          else do
            -- Add raw bytes to the queue
            atomically $ write readQueue (Just bytes)
            loop

  (loop `catch` \(_ :: IOException) -> return ())
    `finally` atomically (write readQueue Nothing)

-- | Thread that takes bytes from write queue and sends them to socket
writerThread :: NS.Socket -> RollingQueue BS.ByteString -> TVar Bool -> IO ()
writerThread sock writeQueue writeClosed = do
  let loop = do
        -- Use STM to atomically check both conditions
        maybeAction <- atomically $ do
          closed <- readTVar writeClosed
          if closed
            then return Nothing -- Signal to exit
            else do
              maybeBytes <- tryRead writeQueue
              case maybeBytes of
                Nothing -> retry -- Block until either data available or writeClosed
                Just bytes -> return (Just bytes)

        case maybeAction of
          Nothing -> return () -- writeClosed was set, exit
          Just (bytes, _discardCount) -> do
            sendAll sock bytes
            loop

  loop `catch` \(_ :: IOException) -> return () -- Socket closed
