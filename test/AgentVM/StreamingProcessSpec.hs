{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AgentVM.StreamingProcessSpec (spec) where

import AgentVM.Interactive (readBytes, tryReadBytes, withInteractive, writeBytes)
import AgentVM.StreamingProcess (Process (..), ProcessPty (..))
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
