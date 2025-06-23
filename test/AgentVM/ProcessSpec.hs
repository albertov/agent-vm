{-# LANGUAGE OverloadedStrings #-}

-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log (AgentVmTrace, vmLogger)
import AgentVM.Monad (VMT, runVMT)
import AgentVM.Process (ProcessState (..), VMProcess (..), checkVMProcess, startVMProcess)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import System.Process.Typed (ExitCode (..), createPipe, proc, setStderr, setStdout, startProcess, stopProcess)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Exception (finally)

-- | Test environment with tracer
testEnv :: AgentVmEnv
testEnv = AgentVmEnv {tracer = vmLogger}

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
      pending

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
      threadDelay 100000 -- 100ms

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
      threadDelay 100000 -- 100ms

      -- Check it has exited with failure
      state <- checkVMProcess vmProcess
      case state of
        ProcessExited (ExitFailure 1) -> return () -- Expected
        ProcessExited ExitSuccess -> panic "Process should have failed"
        ProcessExited (ExitFailure n) -> panic $ "Unexpected exit code: " <> toS (show n :: [Char])
        ProcessRunning -> panic "Process should have exited"

    it "waits for process with timeout" $ do
      pending

  describe "Process error handling" $ do
    it "handles startup failures" $ do
      pending

    it "handles zombie processes" $ do
      pending

    it "cleans up on unexpected termination" $ do
      pending
