{-# LANGUAGE OverloadedStrings #-}

-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Log (AgentVmTrace)
import AgentVM.Process (ProcessState (..), checkVMProcess, startVMProcess)
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import System.Process.Typed (ExitCode (..), proc, startProcess, stopProcess)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Exception (finally)

-- | Test tracer that discards all logs
testTracer :: IOTracer AgentVmTrace
testTracer = IOTracer $ Tracer $ \_ -> return ()

spec :: Spec
spec = describe "AgentVM.Process" $ do
  describe "VM process lifecycle" $ do
    it "starts VM process" $ do
      -- Create a simple test script that runs
      let testScript = "/bin/sh"

      -- Start the process
      process <- startVMProcess testTracer testScript

      -- Check that it's running
      state <- checkVMProcess process
      state `shouldBe` ProcessRunning

      -- Clean up
      stopProcess process

    it "stops VM process gracefully" $ do
      pending

    it "checks if process is running" $ do
      -- Start a simple process
      process <- startProcess (proc "sleep" ["1"])

      -- Check it's running
      checkVMProcess process `shouldReturn` ProcessRunning

      -- Clean up
      stopProcess process

    it "detects when process has exited" $ do
      -- Start a process that exits immediately
      process <- startProcess (proc "true" [])

      -- Wait a bit for it to exit
      threadDelay 100000 -- 100ms

      -- Check it has exited
      state <- checkVMProcess process
      case state of
        ProcessExited ExitSuccess -> return () -- Expected
        ProcessExited (ExitFailure _) -> panic "Process exited with failure"
        ProcessRunning -> panic "Process should have exited"

    it "detects when process has exited with failure" $ do
      -- Start a process that exits with error code
      process <- startProcess (proc "false" [])

      -- Wait a bit for it to exit
      threadDelay 100000 -- 100ms

      -- Check it has exited with failure
      state <- checkVMProcess process
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
