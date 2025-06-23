{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log hiding (ProcessExited)
import AgentVM.Monad (runVMT)
import AgentVM.Process (ProcessState (..), VMProcess (..), VMProcessWithCapture (..), checkVMProcess, stopVMProcess, stopVMProcessCaptured, waitForProcessCaptured, withVMProcessCaptured)
import qualified AgentVM.Process as P
import Control.Concurrent.Thread.Delay (delay)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import System.FilePath ((</>))
import System.IO.Error (IOError (..), IOErrorType (..), ioError, mkIOError, userErrorType)
import System.Process.Typed (ExitCode (..))
import Test.Hspec (Spec, describe, expectationFailure, it, pending, shouldBe, shouldContain, shouldSatisfy)
import UnliftIO (MonadUnliftIO, SomeException)
import qualified UnliftIO as UIO
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef, readIORef)

-- | Test environment with custom tracer that captures traces
testEnvWithCapture :: IORef [AgentVmTrace] -> AgentVmEnv
testEnvWithCapture traceRef =
  AgentVmEnv
    { tracer = IOTracer $ Tracer $ \trace ->
        liftIO $ atomicModifyIORef' traceRef ((,()) . (trace :))
    }

-- | Test environment with tracer
testEnv :: AgentVmEnv
testEnv = AgentVmEnv {tracer = vmLogger}

-- | Helper to get fixture path
fixturePath :: FilePath -> FilePath
fixturePath script = "test/fixtures/ProcessSpec" </> script

defTimeout :: Maybe Integer
defTimeout = Just 1_000_000

defWait :: Integer
defWait = 500_000

spec :: Spec
spec = describe "AgentVM.Process" $ do
  describe "VM process lifecycle" $ do
    it "starts and stops VM process" $ do
      -- Create a simple test script that runs
      let testScript = "/bin/sh"
      -- Start the process using the VMT monad
      runVMT @IO testEnv $
        P.withVMProcess "/bin/sh" [] defTimeout $ \process -> do
          -- Check that it's running
          checkVMProcess process
            >>= liftIO
            . (`shouldBe` ProcessRunning)

    it "stops VM process gracefully" $ do
      -- Start a long-running process
      runVMT @IO testEnv $
        P.withVMProcess "/bin/sh" [] defTimeout $ \process -> do
          -- Check that it's running
          checkVMProcess process
            >>= liftIO
            . (`shouldBe` ProcessRunning)
          _ <- stopVMProcess (Just 100_000) process
          -- Check that it has stopped
          checkVMProcess process
            >>= liftIO
            . ( `shouldSatisfy`
                  \case
                    ProcessExited {} -> True
                    _ -> False
              )

  describe "Process output capture" $ do
    it "captures stdout output" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that outputs to stdout
      runVMT @IO env $
        withFixture "echo_stdout.sh" $
          -- Wait for process to complete
          void . P.waitForProcess defWait

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]

      length outputTraces `shouldBe` 2
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Hello from stdout" -> True; _ -> False)
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Second line to stdout" -> True; _ -> False)

    it "captures stderr output" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that outputs to stderr
      runVMT @IO env $
        withFixture "echo_stderr.sh" $
          void . P.waitForProcess defWait

      -- Check captured traces
      traces <- readIORef traceCapture
      let errorTraces = [t | t@(ProcessError _ _) <- traces]

      length errorTraces `shouldBe` 2
      errorTraces `shouldSatisfy` any (\case ProcessError _ "Error message to stderr" -> True; _ -> False)
      errorTraces `shouldSatisfy` any (\case ProcessError _ "Another error line" -> True; _ -> False)

    it "captures both stdout and stderr output" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that outputs to both
      runVMT @IO env $ withFixture "echo_both.sh" $ \process -> do
        -- Wait for process to complete
        void $ P.waitForProcess defWait process

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
      let errorTraces = [t | t@(ProcessError _ _) <- traces]

      length outputTraces `shouldBe` 3
      length errorTraces `shouldBe` 2

      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Starting process..." -> True; _ -> False)
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Processing data..." -> True; _ -> False)
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Done!" -> True; _ -> False)

      errorTraces `shouldSatisfy` any (\case ProcessError _ "Warning: This is stderr" -> True; _ -> False)
      errorTraces `shouldSatisfy` any (\case ProcessError _ "Error: Something went wrong" -> True; _ -> False)

    it "handles process with exit failure" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that exits with failure
      exitCode <-
        runVMT @IO env $
          withFixture "exit_failure.sh" $
            -- Wait for process to complete
            P.waitForProcess defWait

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
      let errorTraces = [t | t@(ProcessError _ _) <- traces]

      exitCode `shouldBe` Just (ExitFailure 1)
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Attempting operation..." -> True; _ -> False)
      errorTraces `shouldSatisfy` any (\case ProcessError _ "FATAL: Operation failed!" -> True; _ -> False)

    it "handles multiline output with special characters" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with multiline output
      void $ runVMT @IO env $ withFixture "multiline_output.sh" $ \process -> do
        -- Wait for process to complete
        void $ P.waitForProcess defWait process

        -- Check captured traces
        traces <- readIORef traceCapture
        let outputTraces = [t | t@(ProcessOutput _ _) <- traces]

        liftIO $ do
          length outputTraces `shouldBe` 5
          outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Line 1" -> True; _ -> False)
          outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Line 2" -> True; _ -> False)
          outputTraces `shouldSatisfy` any (\case ProcessOutput _ "" -> True; _ -> False)
          outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Line 4 after empty line" -> True; _ -> False)
          outputTraces `shouldSatisfy` any (\case ProcessOutput _ line -> "special chars" `isInfixOf` toS line; _ -> False)

    it "handles rapid output from both streams" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with rapid output
      void $ runVMT @IO env $ withFixture "rapid_output.sh" $ \process -> do
        -- Wait for process to complete
        void $ P.waitForProcess defWait process

        -- Check captured traces
        traces <- readIORef traceCapture
        let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
        let errorTraces = [t | t@(ProcessError _ _) <- traces]

        liftIO $ do
          length outputTraces `shouldBe` 10
          length errorTraces `shouldBe` 10

          -- Verify we captured all lines
          [1 .. 10] `forM_` \i -> do
            let expectedOut = "stdout line " <> show i :: Text
            let expectedErr = "stderr line " <> show i :: Text
            outputTraces `shouldSatisfy` any (\case ProcessOutput _ line -> expectedOut == toS line; _ -> False)
            errorTraces `shouldSatisfy` any (\case ProcessError _ line -> expectedErr == toS line; _ -> False)

    it "handles process with no output" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with no output
      void $ runVMT @IO env $ withFixture "no_output.sh" $ \process -> do
        -- Wait for process to complete
        void $ P.waitForProcess defWait process

        -- Check captured traces (should only have ProcessSpawned)
        traces <- readIORef traceCapture
        let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
        let errorTraces = [t | t@(ProcessError _ _) <- traces]
        let spawnTraces = [t | t@(ProcessSpawned _ _) <- traces]

        liftIO $ do
          length outputTraces `shouldBe` 0
          length errorTraces `shouldBe` 0
          length spawnTraces `shouldBe` 1

    it "traces process spawn event with arguments" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with arguments
      void $ runVMT @IO env $ P.withVMProcess (fixturePath "echo_stdout.sh") ["arg1", "arg2"] defTimeout $ \process -> do
        -- Wait for process to complete
        void $ P.waitForProcess defWait process

        -- Check spawn trace
        traces <- readIORef traceCapture
        let spawnTraces = [t | t@(ProcessSpawned _ _) <- traces]

        liftIO $ do
          length spawnTraces `shouldBe` 1
          case spawnTraces of
            [ProcessSpawned path args] -> do
              toS path `shouldContain` "echo_stdout.sh"
              args `shouldBe` ["arg1", "arg2"]
            _ -> expectationFailure "Expected exactly one ProcessSpawned trace"

  describe "Process output capture functions" $ do
    it "captures stdout and stderr with waitForProcessCaptured" $ do
      -- Run process that outputs to both
      runVMT @IO testEnv $ do
        process <- P.startLoggedProcessWithCapture (fixturePath "echo_both.sh") []

        -- Wait and capture output
        result <- waitForProcessCaptured defWait process

        case result of
          Just (ExitSuccess, stdout, stderr) -> liftIO $ do
            -- Check stdout content
            toS stdout `shouldContain` "Starting process..."
            toS stdout `shouldContain` "Processing data..."
            toS stdout `shouldContain` "Done!"

            -- Check stderr content
            toS stderr `shouldContain` "Warning: This is stderr"
            toS stderr `shouldContain` "Error: Something went wrong"
          _ -> liftIO $ expectationFailure "Expected successful exit with output"

    it "captures output from failing process with waitForProcessCaptured" $ do
      -- Run process that fails
      runVMT @IO testEnv $ do
        process <- P.startLoggedProcessWithCapture (fixturePath "exit_failure.sh") []

        -- Wait and capture output
        result <- waitForProcessCaptured defWait process

        case result of
          Just (ExitFailure 1, stdout, stderr) -> liftIO $ do
            -- Check captured output
            toS stdout `shouldContain` "Attempting operation..."
            toS stderr `shouldContain` "FATAL: Operation failed!"
          _ -> liftIO $ expectationFailure "Expected exit failure with code 1"

    it "captures output with stopVMProcessCaptured" $ do
      -- Run a long-running process
      runVMT @IO testEnv $ do
        process <- P.startLoggedProcessWithCapture (fixturePath "rapid_output.sh") []

        -- Let it run for a bit
        liftIO $ delay 100_000 -- 0.1 seconds

        -- Stop and capture output
        (exitCode, stdout, stderr) <- stopVMProcessCaptured (Just 1_000_000) process

        liftIO $ do
          -- Should have captured at least some output
          stdout `shouldSatisfy` (not . null . lines)
          stderr `shouldSatisfy` (not . null . lines)

    it "captures output with withVMProcessCaptured" $ do
      -- Use the bracket-style function
      (result, stdout, stderr) <- runVMT @IO testEnv $
        withVMProcessCaptured (fixturePath "echo_both.sh") [] defTimeout $ \process -> do
          -- Wait for completion
          void $ P.waitForProcess defWait (P.VMProcess (P.getProcessCapture process) (P.waitIOThreadsCapture process))
          pure "test result"

      -- Check result and captured output
      result `shouldBe` "test result"
      toS stdout `shouldContain` "Starting process..."
      toS stdout `shouldContain` "Done!"
      toS stderr `shouldContain` "Warning: This is stderr"

    it "handles empty output with captured functions" $ do
      -- Run process with no output
      runVMT @IO testEnv $ do
        process <- P.startLoggedProcessWithCapture (fixturePath "no_output.sh") []

        -- Wait and capture output
        result <- waitForProcessCaptured defWait process

        case result of
          Just (ExitSuccess, stdout, stderr) -> liftIO $ do
            stdout `shouldBe` ""
            stderr `shouldBe` ""
          _ -> liftIO $ expectationFailure "Expected successful exit"

    it "preserves line order in captured output" $ do
      -- Run process with numbered lines
      runVMT @IO testEnv $ do
        process <- P.startLoggedProcessWithCapture (fixturePath "rapid_output.sh") []

        -- Wait for completion
        result <- waitForProcessCaptured defWait process

        case result of
          Just (ExitSuccess, stdout, stderr) -> liftIO $ do
            let stdoutLines = lines stdout
            let stderrLines = lines stderr

            -- Verify all 10 lines are captured in order
            length stdoutLines `shouldBe` 10
            length stderrLines `shouldBe` 10

            -- Check order
            forM_ (zip [1 .. 10] stdoutLines) $ \(i, line) ->
              line `shouldBe` ("stdout line " <> show i)
            forM_ (zip [1 .. 10] stderrLines) $ \(i, line) ->
              line `shouldBe` ("stderr line " <> show i)
          _ -> liftIO $ expectationFailure "Expected successful exit"

    it "handles exceptions in withVMProcessCaptured" $ do
      -- Test that output is still captured even if action throws
      result <- UIO.try $
        runVMT @IO testEnv $
          withVMProcessCaptured (fixturePath "echo_both.sh") [] defTimeout $ \process -> do
            -- Wait a bit to ensure some output is generated
            liftIO $ delay 100_000
            -- Throw an exception
            liftIO $ ioError $ mkIOError userErrorType "Test exception" Nothing Nothing

      case result of
        Left (e :: SomeException) -> show e `shouldContain` "Test exception"
        Right _ -> expectationFailure "Expected an exception"

  describe "Process error handling" $ do
    it "handles startup failures" $ do
      -- Try to start a non-existent executable
      result <- UIO.try $ runVMT @IO testEnv $ do
        P.withVMProcess "/non/existent/executable" [] defTimeout $ \process -> do
          -- This should not be reached
          liftIO $ expectationFailure "Process should have failed to start"

      case result of
        Left (_ :: SomeException) -> pure () -- Expected failure
        Right _ -> expectationFailure "Expected process startup to fail"

    it "handles zombie processes" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that creates zombies
      exitCode <- runVMT @IO env $ withFixture "zombie_process.sh" $ \process -> do
        -- Wait for process to complete
        P.waitForProcess defWait process

      -- Check that process completed normally
      exitCode `shouldBe` Just ExitSuccess

      -- Check traces were captured properly
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]

      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Creating zombie process..." -> True; _ -> False)

    it "cleans up on unexpected termination" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that terminates unexpectedly
      result <- runVMT @IO env $ withFixture "unexpected_termination.sh" $ \process -> do
        -- Wait for process to complete (it will be killed)
        P.waitForProcess defWait process

      -- Check that process was terminated (exit code should indicate termination)
      case result of
        Just (ExitFailure code) | code < 0 -> pure () -- Negative exit codes indicate signals
        Just ExitSuccess -> expectationFailure "Expected process to be terminated, but it exited successfully"
        Just (ExitFailure code) -> expectationFailure $ "Expected signal termination, got exit code: " <> show code
        Nothing -> pure () -- Timeout is also acceptable for this test

      -- Check traces were captured
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]

      -- Should capture some output before termination
      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Process starting normally..." -> True; _ -> False)

withFixture :: (MonadTrace AgentVmTrace m, MonadUnliftIO m) => FilePath -> (VMProcess -> m a) -> m a
withFixture scriptText =
  P.withVMProcess
    (fixturePath scriptText)
    []
    defTimeout
