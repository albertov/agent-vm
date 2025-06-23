{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log hiding (ProcessExited)
import AgentVM.Monad (VMT, runVMT)
import AgentVM.Process (ProcessState (..), VMProcess (..), checkVMProcess, startLoggedProcess, startVMProcess, stopVMProcess)
import qualified AgentVM.Process as P
import Control.Concurrent.Thread.Delay (delay)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Process.Typed (ExitCode (..), createPipe, getExitCode, proc, setStderr, setStdout, startProcess, stopProcess, waitExitCode)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldContain, shouldReturn, shouldSatisfy)
import UnliftIO.Exception (finally)

-- | Test environment with custom tracer that captures traces
testEnvWithCapture :: IORef [AgentVmTrace] -> AgentVmEnv
testEnvWithCapture traceRef = AgentVmEnv {tracer = IOTracer $ Tracer $ \trace -> liftIO $ modifyIORef' traceRef (trace :)}

-- | Test environment with tracer
testEnv :: AgentVmEnv
testEnv = AgentVmEnv {tracer = vmLogger}

-- | Helper to get fixture path
fixturePath :: FilePath -> FilePath
fixturePath script = "test/fixtures/ProcessSpec" </> script

spec :: Spec
spec = describe "AgentVM.Process" $ do
  describe "VM process lifecycle" $ do
    it "starts VM process" $ do
      -- Create a simple test script that runs
      let testScript = "/bin/sh"

      -- Start the process using the VMT monad
      process <- runVMT testEnv $ startVMProcess testScript

      -- Check that it's running
      state <- checkVMProcess process
      state `shouldBe` ProcessRunning

    -- TODO: Clean up once stopVMProcess is implemented
    -- For now, we'll skip cleanup since VMProcess is opaque

    it "stops VM process gracefully" $ do
      -- Start a long-running process
      let testScript = "/bin/sh"

      -- Start the process using the VMT monad
      process <- runVMT testEnv $ startVMProcess testScript

      -- Check that it's running
      checkVMProcess process `shouldReturn` ProcessRunning

      -- Stop the process
      runVMT testEnv $ stopVMProcess process

      -- Small delay to ensure process has time to stop
      delay 100000 -- 100ms

      -- Check that it has stopped
      state <- checkVMProcess process
      case state of
        ProcessExited _ -> return () -- Expected
        ProcessRunning -> panic "Process should have stopped"

    it "checks if process is running" $ do
      -- Start a simple process with pipes
      process <-
        startProcess $
          proc "sleep" ["1"]
            & setStdout createPipe
            & setStderr createPipe

      -- Create a VMProcess wrapper
      let vmProcess = VMProcess process

      -- Check it's running
      checkVMProcess vmProcess `shouldReturn` ProcessRunning

      -- Clean up
      stopProcess process

    it "detects when process has exited" $ do
      -- Start a process that exits immediately with pipes
      process <-
        startProcess $
          proc "true" []
            & setStdout createPipe
            & setStderr createPipe

      -- Create a VMProcess wrapper
      let vmProcess = VMProcess process

      -- Wait a bit for it to exit
      delay 100000 -- 100ms

      -- Check it has exited
      state <- checkVMProcess vmProcess
      case state of
        ProcessExited ExitSuccess -> return () -- Expected
        ProcessExited (ExitFailure _) -> panic "Process exited with failure"
        ProcessRunning -> panic "Process should have exited"

    it "detects when process has exited with failure" $ do
      -- Start a process that exits with error code with pipes
      process <-
        startProcess $
          proc "false" []
            & setStdout createPipe
            & setStderr createPipe

      -- Create a VMProcess wrapper
      let vmProcess = VMProcess process

      -- Wait a bit for it to exit
      delay 100000 -- 100ms

      -- Check it has exited with failure
      state <- checkVMProcess vmProcess
      case state of
        ProcessExited (ExitFailure 1) -> return () -- Expected
        ProcessExited ExitSuccess -> panic "Process should have failed"
        ProcessExited (ExitFailure n) -> panic $ "Unexpected exit code: " <> toS (show n :: [Char])
        ProcessRunning -> panic "Process should have exited"

    it "waits for process with timeout" $ do
      -- Test case 1: Process that exits before timeout
      process1 <-
        startProcess $
          proc "sleep" ["0.1"] -- 100ms sleep
            & setStdout createPipe
            & setStderr createPipe

      -- Wait with generous timeout (1 second)
      result1 <- P.waitForProcess 1000000 process1
      result1 `shouldBe` Just ExitSuccess

      -- Test case 2: Process that times out
      process2 <-
        startProcess $
          proc "sleep" ["5"] -- 5 second sleep
            & setStdout createPipe
            & setStderr createPipe

      -- Wait with short timeout (100ms)
      result2 <- P.waitForProcess 100000 process2
      result2 `shouldBe` Nothing

      -- Clean up the long-running process
      stopProcess process2

  describe "Process output capture" $ do
    it "captures stdout output" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process that outputs to stdout
      process <- runVMT env $ startLoggedProcess (fixturePath "echo_stdout.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

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
      process <- runVMT env $ startLoggedProcess (fixturePath "echo_stderr.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

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
      process <- runVMT env $ startLoggedProcess (fixturePath "echo_both.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

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
      process <- runVMT env $ startLoggedProcess (fixturePath "exit_failure.sh") []

      -- Wait for process to complete
      exitCode <- waitExitCode process
      exitCode `shouldBe` ExitFailure 1

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
      let errorTraces = [t | t@(ProcessError _ _) <- traces]

      outputTraces `shouldSatisfy` any (\case ProcessOutput _ "Attempting operation..." -> True; _ -> False)
      errorTraces `shouldSatisfy` any (\case ProcessError _ "FATAL: Operation failed!" -> True; _ -> False)

    it "handles multiline output with special characters" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with multiline output
      process <- runVMT env $ startLoggedProcess (fixturePath "multiline_output.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]

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
      process <- runVMT env $ startLoggedProcess (fixturePath "rapid_output.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 200000 -- 200ms

      -- Check captured traces
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
      let errorTraces = [t | t@(ProcessError _ _) <- traces]

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
      process <- runVMT env $ startLoggedProcess (fixturePath "no_output.sh") []

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Small delay to ensure async output capture completes
      delay 100000 -- 100ms

      -- Check captured traces (should only have ProcessSpawned)
      traces <- readIORef traceCapture
      let outputTraces = [t | t@(ProcessOutput _ _) <- traces]
      let errorTraces = [t | t@(ProcessError _ _) <- traces]
      let spawnTraces = [t | t@(ProcessSpawned _ _) <- traces]

      length outputTraces `shouldBe` 0
      length errorTraces `shouldBe` 0
      length spawnTraces `shouldBe` 1

    it "traces process spawn event with arguments" $ do
      -- Create trace capture
      traceCapture <- newIORef []
      let env = testEnvWithCapture traceCapture

      -- Run process with arguments
      process <- runVMT env $ startLoggedProcess (fixturePath "echo_stdout.sh") ["arg1", "arg2"]

      -- Wait for process to complete
      _ <- waitExitCode process

      -- Check spawn trace
      traces <- readIORef traceCapture
      let spawnTraces = [t | t@(ProcessSpawned _ _) <- traces]

      length spawnTraces `shouldBe` 1
      case spawnTraces of
        [ProcessSpawned path args] -> do
          toS path `shouldContain` "echo_stdout.sh"
          args `shouldBe` ["arg1", "arg2"]
        _ -> panic "Expected exactly one ProcessSpawned trace"

  describe "Process error handling" $ do
    it "handles startup failures" $ do
      pending

    it "handles zombie processes" $ do
      pending

    it "cleans up on unexpected termination" $ do
      pending
