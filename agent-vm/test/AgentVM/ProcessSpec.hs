-- | Tests for process management
module AgentVM.ProcessSpec (spec) where

import AgentVM.Log (LogAction (LogAction))
import AgentVM.Process (checkVMProcess, startVMProcess)
import Protolude
import System.Process.Typed (proc, startProcess, stopProcess)
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn)
import UnliftIO.Exception (finally)

spec :: Spec
spec = describe "AgentVM.Process" $ do
  describe "VM process lifecycle" $ do
    it "starts VM process" $ do
      -- Create a simple test script that runs
      let testScript = "/bin/sh"
      let mockLogger = LogAction $ \_ -> return ()

      -- Start the process
      process <- startVMProcess mockLogger testScript

      -- Check that it's running
      isRunning <- checkVMProcess process
      isRunning `shouldBe` True

      -- Clean up
      stopProcess process

    it "stops VM process gracefully" $ do
      pending

    it "checks if process is running" $ do
      -- Start a simple process
      process <- startProcess (proc "sleep" ["1"])

      -- Check it's running
      checkVMProcess process `shouldReturn` True

      -- Clean up
      stopProcess process

    it "waits for process with timeout" $ do
      pending

  describe "Process error handling" $ do
    it "handles startup failures" $ do
      pending

    it "handles zombie processes" $ do
      pending

    it "cleans up on unexpected termination" $ do
      pending
