{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgentVM.StreamingProcessSpec (spec) where

import AgentVM.Interactive (parseEscapeKey, readBytes, tryReadBytes, withInteractive, writeBytes)
import AgentVM.StreamingProcess (Process (..), ProcessPty (..))
import qualified Data.ByteString as BS
import Protolude hiding (try)
import Test.Hspec
import UnliftIO (timeout, try)

spec :: Spec
spec = describe "StreamingProcess" $ do
  describe "when process exits immediately" $ do
    it "should not hang when trying to read" $ do
      -- Test with a command that exits immediately
      result <- timeout 1000000 $ -- 1 second timeout
        withInteractive 10 (Process "false" []) $ \sp -> do
          -- This should throw EOF error, not hang
          try (readBytes sp) :: IO (Either SomeException ByteString)

      result `shouldSatisfy` isJust
      case result of
        Just (Left e) ->
          e `shouldSatisfy` \ex ->
            "closed" `isInfixOf` show ex
        _ -> expectationFailure "Expected an exception"

    it "should handle non-existent commands gracefully" $ do
      -- Test with a command that doesn't exist
      result <- try $ withInteractive 10 (Process "/nonexistent/command" []) $ \_ ->
        pure ()

      case result of
        Left (_ :: SomeException) -> pure () -- Expected
        Right _ -> expectationFailure "Expected exception for non-existent command"

  describe "normal operation" $ do
    it "should stream data from echo command" $ do
      withInteractive 10 (Process "echo" ["hello"]) $ \sp -> do
        bytes <- readBytes sp
        bytes `shouldBe` "hello\n"

    it "should handle bidirectional communication with cat" $ do
      withInteractive 10 (Process "cat" []) $ \sp -> do
        writeBytes sp "test input\n"
        result <- readBytes sp
        result `shouldBe` "test input\n"

  describe "tryReadBytes" $ do
    it "should return Nothing on EOF without throwing" $ do
      withInteractive 10 (Process "echo" ["hello"]) $ \sp -> do
        -- First read should get the output
        Just bytes <- tryReadBytes sp
        bytes `shouldBe` "hello\n"

        -- Subsequent reads should return Nothing (EOF) without throwing
        result1 <- tryReadBytes sp
        result1 `shouldBe` Nothing

        result2 <- tryReadBytes sp
        result2 `shouldBe` Nothing

  describe "stdin forwarding" $ do
    it "should properly forward stdin input to the process" $ do
      withInteractive 10 (Process "cat" []) $ \sp -> do
        -- Test sending multiple chunks of data
        writeBytes sp "line1\n"
        result1 <- readBytes sp
        result1 `shouldBe` "line1\n"

        writeBytes sp "line2\n"
        result2 <- readBytes sp
        result2 `shouldBe` "line2\n"

    it "should handle partial line input correctly" $ do
      withInteractive 10 (Process "cat" []) $ \sp -> do
        -- Send partial input without newline
        writeBytes sp "partial"
        writeBytes sp " input\n"

        -- Should receive the full line
        result <- readBytes sp
        result `shouldBe` "partial input\n"

    it "should not hang when process is waiting for input" $ do
      -- This test verifies that stdin forwarding works properly
      -- and doesn't hang when the process is waiting for input
      result <- timeout 1000000 $ -- 1 second timeout
        withInteractive 10 (Process "sh" ["-c", "read line; echo \"Got: $line\""]) $ \sp -> do
          -- Write input after a small delay to ensure process is ready
          writeBytes sp "test input\n"

          -- Should receive the echoed output
          output <- readBytes sp
          output `shouldBe` "Got: test input\n"

      result `shouldSatisfy` isJust

    it "should handle multiple rounds of interactive I/O" $ do
      withInteractive 10 (Process "sh" ["-c", "while read line; do echo \"Echo: $line\"; done"]) $ \sp -> do
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

  describe "interactive script" $ do
    it "should handle interactive command session" $ do
      withInteractive 10 (ProcessPty "test/fixtures/interact.sh" []) $ \sp -> do
        -- Read initial output which includes welcome message and prompt
        initialOutput <- readBytes sp
        show initialOutput `shouldSatisfy` ("Interactive test script started" `isInfixOf`)
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
            terminationOutput `shouldBe` "Interactive test script terminated\n"
          else
            quitOutput `shouldBe` "Goodbye!\nInteractive test script terminated\n"

        -- Further reads should fail with EOF
        tryReadBytes sp >>= (`shouldBe` Nothing)

  describe "escape sequence handling" $ do
    it "should handle control character sequences" $ do
      withInteractive 10 (ProcessPty "test/fixtures/escape-test-simple.sh" []) $ \sp -> do
        -- Read initial output
        initialOutput <- readBytes sp
        show initialOutput `shouldSatisfy` ("Escape test ready" `isInfixOf`)
        show initialOutput `shouldSatisfy` ("> " `isInfixOf`)

        -- Send regular text
        writeBytes sp "test\n"
        response1 <- readBytes sp
        response1 `shouldSatisfy` (\out -> "Test response" `isInfixOf` show out)

        -- Send control character (Ctrl-W = byte 23)
        let ctrlW = BS.pack [23]
        writeBytes sp (ctrlW <> "x\n")
        response2 <- readBytes sp
        -- Should get echo response for the control sequence
        response2 `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Send quit to exit cleanly
        writeBytes sp "quit\n"
        quitOutput <- readBytes sp
        quitOutput `shouldSatisfy` (\out -> "Goodbye" `isInfixOf` show out)

    it "should parse escape key bytes correctly" $ do
      withInteractive 10 (ProcessPty "test/fixtures/escape-test-simple.sh" []) $ \sp -> do
        -- Read initial output
        _ <- readBytes sp

        -- Test different control characters
        let ctrlA = BS.pack [1] -- Ctrl-A
        let ctrlC = BS.pack [3] -- Ctrl-C
        let ctrlZ = BS.pack [26] -- Ctrl-Z

        -- Send Ctrl-A
        writeBytes sp (ctrlA <> "hello\n")
        response1 <- readBytes sp
        response1 `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Send Ctrl-C
        writeBytes sp (ctrlC <> "hello\n")
        response2 <- readBytes sp
        response2 `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Send Ctrl-Z
        writeBytes sp (ctrlZ <> "hello\n")
        response3 <- readBytes sp
        response3 `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Clean exit
        writeBytes sp "quit\n"
        void $ readBytes sp

    it "should handle escape sequence detection with parseEscapeKey integration" $ do
      -- Test that parseEscapeKey matches the byte values we're sending
      parseEscapeKey "ctrl-w" `shouldBe` 23
      parseEscapeKey "ctrl-a" `shouldBe` 1
      parseEscapeKey "ctrl-c" `shouldBe` 3
      parseEscapeKey "ctrl-z" `shouldBe` 26

      withInteractive 10 (ProcessPty "test/fixtures/escape-test-simple.sh" []) $ \sp -> do
        -- Read initial output
        void $ readBytes sp

        -- Use parseEscapeKey to get the correct byte value
        let escapeKeyByte = parseEscapeKey "ctrl-w"
        let ctrlWBytes = BS.pack [escapeKeyByte]

        -- Send the escape key sequence
        writeBytes sp (ctrlWBytes <> "hello\n")
        response <- readBytes sp
        response `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Clean exit
        writeBytes sp "quit\n"
        void $ readBytes sp

  describe "escape sequence exit functionality" $ do
    it "should exit when escape key + x is sent" $ do
      -- This test should fail because the escape sequence detection
      -- only works in interactWith, not in direct Interactive usage
      withInteractive 10 (ProcessPty "test/fixtures/infinite-loop-test.sh" []) $ \sp -> do
        -- Read initial output
        initialOutput <- readBytes sp
        show initialOutput `shouldSatisfy` ("Infinite loop test started" `isInfixOf`)

        -- Send a regular command first to confirm the script is responding
        writeBytes sp "test input\n"
        response1 <- readBytes sp
        response1 `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Now send the escape sequence: Ctrl-W followed by 'x'
        let escapeKeyByte = parseEscapeKey "ctrl-w" -- Should be 23
        let escapeSequence = BS.pack [escapeKeyByte, 120] -- Ctrl-W + 'x'
        writeBytes sp escapeSequence

        -- The session should exit quickly due to escape sequence detection
        -- But this will likely fail because the escape detection doesn't happen here
        result <- timeout 1000000 $ do
          -- 1 second timeout
          -- Try to read - should get EOF quickly if escape sequence worked
          tryReadBytes sp

        case result of
          Nothing -> expectationFailure "Escape sequence should have terminated the session within 1 second"
          Just Nothing -> pure () -- EOF received, escape sequence worked
          Just (Just _) -> expectationFailure "Should have received EOF after escape sequence"

    it "should NOT exit when just escape key is sent without x" $ do
      -- This test verifies the escape sequence needs both parts
      withInteractive 10 (ProcessPty "test/fixtures/infinite-loop-test.sh" []) $ \sp -> do
        -- Read initial output
        void $ readBytes sp

        -- Send just the escape key without 'x'
        let escapeKeyByte = parseEscapeKey "ctrl-w"
        writeBytes sp (BS.pack [escapeKeyByte] <> "test\n")

        -- Should get normal echo response, not exit
        response <- readBytes sp
        response `shouldSatisfy` (\out -> "Echo:" `isInfixOf` show out)

        -- Clean exit
        writeBytes sp "quit\n"
        void $ readBytes sp

    it "should exit within reasonable time when proper escape sequence is sent" $ do
      -- This test will likely timeout/fail, exposing the bug
      result <- timeout 2000000 $ do
        -- 2 second timeout
        withInteractive 10 (ProcessPty "test/fixtures/infinite-loop-test.sh" []) $ \sp -> do
          -- Read initial output
          void $ readBytes sp

          -- Send escape sequence immediately
          let escapeKeyByte = parseEscapeKey "ctrl-w"
          let escapeSequence = BS.pack [escapeKeyByte, 120] -- Ctrl-W + 'x'
          writeBytes sp escapeSequence

          -- Try to read - should get EOF if escape sequence detection works
          -- This will likely hang because escape detection doesn't work
          result <- tryReadBytes sp
          case result of
            Nothing -> pure () -- EOF - escape sequence worked
            Just _ -> expectationFailure "Expected EOF after escape sequence"

      case result of
        Nothing -> expectationFailure "Escape sequence test timed out - escape detection may not be working"
        Just _ -> pure () -- Test completed within timeout

-- TODO: Fix stderr capture with proper EOF handling
-- it "should capture stderr output" $ do
--   withInteractive 10 (Process "sh" ["-c", "echo 'stdout message'; echo 'stderr message' >&2"]) $ \sp -> do
--     -- Should receive both stdout and stderr
--     output1 <- readBytes sp
--     output2 <- readBytes sp
--
--     -- Order might vary, but we should get both messages
--     let outputs = [output1, output2]
--     outputs `shouldSatisfy` ("stdout message\n" `elem`)
--     outputs `shouldSatisfy` ("stderr message\n" `elem`)
