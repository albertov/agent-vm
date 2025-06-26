{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgentVM.StreamingSocketSpec (spec) where

import AgentVM.Interactive (readBytes, tryReadBytes, withInteractive, writeBytes)
import AgentVM.StreamingSocket (Socket (..))
import Control.Concurrent.Thread.Delay (delay)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), accept, bind, close, listen, socket)
import Network.Socket.ByteString (recv, sendAll)
import Protolude hiding (finally, try, withAsync)
import Test.Hspec
import UnliftIO (timeout, try)
import UnliftIO.Async (withAsync)
import UnliftIO.Exception (catchAny, finally)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = around withTempDir $ describe "StreamingSocket" $ do
  describe "when socket closes immediately" $ do
    it "should not hang when trying to read" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      -- Create a server that accepts and immediately closes
      withAsync (runClosingServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        result <- timeout 1000000 $ -- 1 second timeout
          withInteractive 10 (Socket socketPath) $ \sp -> do
            -- This should throw EOF error, not hang
            try (readBytes sp) :: IO (Either SomeException ByteString)

        result `shouldSatisfy` isJust
        case result of
          Just (Left e) ->
            e `shouldSatisfy` \ex ->
              "closed" `isInfixOf` show ex
          _ -> expectationFailure "Expected an exception"

    it "should handle non-existent socket gracefully" $ \_ -> do
      -- Test with a socket that doesn't exist
      result <- try $ withInteractive 10 (Socket "/nonexistent/socket.sock") $ \_ ->
        pure ()

      case result of
        Left (_ :: SomeException) -> pure () -- Expected
        Right _ -> expectationFailure "Expected exception for non-existent socket"

  describe "normal operation" $ do
    it "should stream data from echo server" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runEchoServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          writeBytes sp "hello\n"
          bytes <- readBytes sp
          bytes `shouldBe` "hello\n"

    it "should handle bidirectional communication" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runEchoServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          writeBytes sp "test input\n"
          result <- readBytes sp
          result `shouldBe` "test input\n"

  describe "tryReadBytes" $ do
    it "should return Nothing on EOF without throwing" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runOneMessageServer socketPath "hello\n") $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          -- First read should get the output
          Just bytes <- tryReadBytes sp
          bytes `shouldBe` "hello\n"

          -- Subsequent reads should return Nothing (EOF) without throwing
          result1 <- tryReadBytes sp
          result1 `shouldBe` Nothing

          result2 <- tryReadBytes sp
          result2 `shouldBe` Nothing

  describe "stdin forwarding" $ do
    it "should properly forward data to the socket" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runEchoServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          -- Test sending multiple chunks of data
          writeBytes sp "line1\n"
          result1 <- readBytes sp
          result1 `shouldBe` "line1\n"

          writeBytes sp "line2\n"
          result2 <- readBytes sp
          result2 `shouldBe` "line2\n"

    it "should handle partial line input correctly" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runBufferingEchoServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          -- Send partial input without newline
          writeBytes sp "partial"
          writeBytes sp " input\n"

          -- Should receive the full line
          result <- readBytes sp
          result `shouldBe` "partial input\n"

    it "should not hang when server is waiting for input" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runInteractiveServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        result <- timeout 1000000 $ -- 1 second timeout
          withInteractive 10 (Socket socketPath) $ \sp -> do
            -- Write input after a small delay to ensure server is ready
            writeBytes sp "test input\n"

            -- Should receive the echoed output
            output <- readBytes sp
            output `shouldBe` "Got: test input\n"

        result `shouldSatisfy` isJust

    it "should handle multiple rounds of interactive I/O" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runPrefixEchoServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          -- First round
          writeBytes sp "first\n"
          result1 <- readBytes sp
          result1 `shouldBe` "Echo: first\n"

          -- Second round
          writeBytes sp "second\n"
          result2 <- readBytes sp
          result2 `shouldBe` "Echo: second\n"

          -- Third round
          writeBytes sp "third\n"
          result3 <- readBytes sp
          result3 `shouldBe` "Echo: third\n"

  describe "interactive protocol" $ do
    it "should handle interactive command session" $ \tmpDir -> do
      let socketPath = tmpDir <> "/test.sock"

      withAsync (runCommandServer socketPath) $ \_ -> do
        -- Give server time to start
        delay 10000 -- 10ms
        withInteractive 10 (Socket socketPath) $ \sp -> do
          -- Read initial output which includes welcome message and prompt
          initialOutput <- readBytes sp
          show initialOutput `shouldSatisfy` ("Interactive test server started" `isInfixOf`)
          show initialOutput `shouldSatisfy` ("> " `isInfixOf`)

          -- Test echo command
          writeBytes sp "echo hello world\n"
          echoOutput <- readBytes sp
          -- The response might come with or without the prompt
          if echoOutput == "Echo: hello world\n"
            then do
              -- Read the prompt separately
              prompt <- readBytes sp
              prompt `shouldBe` "> "
            else
              echoOutput `shouldBe` "Echo: hello world\n> "

          -- Test quit command
          writeBytes sp "quit\n"
          quitOutput <- readBytes sp
          -- Might get both messages together or separately
          if quitOutput == "Goodbye!\n"
            then do
              -- Read termination message separately
              terminationOutput <- readBytes sp
              terminationOutput `shouldBe` "Interactive test server terminated\n"
            else
              quitOutput `shouldBe` "Goodbye!\nInteractive test server terminated\n"

          -- Further reads should fail with EOF
          tryReadBytes sp >>= (`shouldBe` Nothing)

-- Helper function to provide temp directory fixture
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "streaming-socket-test"

-- Server helper functions

-- | Server that accepts connection and immediately closes it
runClosingServer :: FilePath -> IO ()
runClosingServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock
  close clientSock
  close serverSock

-- | Simple echo server that echoes back exactly what it receives
runEchoServer :: FilePath -> IO ()
runEchoServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  let loop = do
        bytes <- recv clientSock 4096
        if BS.null bytes
          then pure ()
          else do
            sendAll clientSock bytes
            loop

  loop `finally` do
    close clientSock
    close serverSock

-- | Server that sends one message then closes
runOneMessageServer :: FilePath -> ByteString -> IO ()
runOneMessageServer socketPath message = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  sendAll clientSock message
  close clientSock
  close serverSock

-- | Echo server that buffers until newline
runBufferingEchoServer :: FilePath -> IO ()
runBufferingEchoServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  let loop buffer = do
        bytes <- recv clientSock 4096
        if BS.null bytes
          then pure ()
          else do
            let newBuffer = buffer <> bytes
            case BS8.elemIndex '\n' newBuffer of
              Nothing -> loop newBuffer
              Just idx -> do
                let (line, rest) = BS.splitAt (idx + 1) newBuffer
                sendAll clientSock line
                if BS.null rest
                  then loop BS.empty
                  else loop rest

  loop BS.empty `finally` do
    close clientSock
    close serverSock

-- | Interactive server that reads a line and responds with "Got: <line>"
runInteractiveServer :: FilePath -> IO ()
runInteractiveServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  let readLine buffer = do
        bytes <- recv clientSock 4096
        if BS.null bytes
          then pure Nothing
          else do
            let newBuffer = buffer <> bytes
            case BS8.elemIndex '\n' newBuffer of
              Nothing -> readLine newBuffer
              Just idx -> pure $ Just $ BS.take idx newBuffer

  maybeLine <- readLine BS.empty
  case maybeLine of
    Just line -> sendAll clientSock ("Got: " <> line <> "\n")
    Nothing -> pure ()

  close clientSock
  close serverSock

-- | Echo server that prefixes each line with "Echo: "
runPrefixEchoServer :: FilePath -> IO ()
runPrefixEchoServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  let loop buffer = do
        bytes <- recv clientSock 4096
        if BS.null bytes
          then pure ()
          else do
            let newBuffer = buffer <> bytes
            case BS8.elemIndex '\n' newBuffer of
              Nothing -> loop newBuffer
              Just idx -> do
                let (line, rest) = BS.splitAt idx newBuffer
                sendAll clientSock ("Echo: " <> line <> "\n")
                if BS.null (BS.drop 1 rest)
                  then loop BS.empty
                  else loop (BS.drop 1 rest)

  loop BS.empty `catchAny` (\_ -> pure ()) `finally` do
    close clientSock
    close serverSock

-- | Command server that handles echo and quit commands
runCommandServer :: FilePath -> IO ()
runCommandServer socketPath = do
  serverSock <- socket AF_UNIX Stream 0
  bind serverSock (SockAddrUnix socketPath)
  listen serverSock 1
  (clientSock, _) <- accept serverSock

  -- Send welcome message
  sendAll clientSock "Interactive test server started. Commands: echo <text>, quit\n> "

  let readLine buffer = do
        bytes <- recv clientSock 4096
        if BS.null bytes
          then pure Nothing
          else do
            let newBuffer = buffer <> bytes
            case BS8.elemIndex '\n' newBuffer of
              Nothing -> readLine newBuffer
              Just idx -> pure $ Just $ BS.take idx newBuffer

  let loop = do
        maybeLine <- readLine BS.empty
        case maybeLine of
          Nothing -> pure ()
          Just line -> do
            let lineText = decodeUtf8With lenientDecode line
            case words lineText of
              ("echo" : rest) -> do
                sendAll clientSock ("Echo: " <> encodeUtf8 (unwords rest) <> "\n> ")
                loop
              ["quit"] -> do
                sendAll clientSock "Goodbye!\nInteractive test server terminated\n"
              _ -> do
                sendAll clientSock ("Unknown command: " <> line <> "\n> ")
                loop

  loop `finally` do
    close clientSock
    close serverSock
