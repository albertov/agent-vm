{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log hiding (ProcessExited)
import AgentVM.Monad (runVMT)
import AgentVM.Process (ProcessState (..), VMProcess (..), checkVMProcess, stopVMProcess)
import qualified AgentVM.Process as P
import Control.Concurrent.Thread.Delay (delay)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import System.FilePath ((</>))
import System.Process.Typed (ExitCode (..))
import Test.Hspec (Spec, describe, expectationFailure, it, pending, shouldBe, shouldContain, shouldSatisfy)
import UnliftIO (MonadUnliftIO)
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
defTimeout = Just 2_000_000

defWait :: Int
defWait = 2_000_000

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

  describe "Process error handling" $ do
    it "handles startup failures" $ do
      pending

    it "handles zombie processes" $ do
      pending

    it "cleans up on unexpected termination" $ do
      pending

withFixture :: (MonadTrace AgentVmTrace m, MonadUnliftIO m) => FilePath -> (VMProcess -> m a) -> m a
withFixture scriptText =
  P.withVMProcess
    (fixturePath scriptText)
    []
    defTimeout
