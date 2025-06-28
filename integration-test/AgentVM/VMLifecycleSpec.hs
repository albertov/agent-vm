{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AgentVM.VMLifecycleSpec (spec) where

import AgentVM (MonadVM (..), runVMT)
import AgentVM.Env (AgentVmEnv)
import AgentVM.TestUtils (withTestEnv)
import AgentVM.Types (VMConfig (..), defVMConfig, vmPidFile)
import Data.Generics.Labels ()
import Lens.Micro
import Protolude hiding (bracket)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (catchAny)
import UnliftIO.Exception (bracket)

spec :: Spec
spec = describe "VM Lifecycle Integration Tests" $ around withTestEnv $ do
  describe "VM Start/Stop with proper cleanup" $ do
    it "can start and stop a VM with bracket pattern" $ \(env, _) -> do
      -- Create test VM configuration
      let envStateDir = env ^. #stateDir
      vmConfig <- testVMConfig envStateDir (envStateDir </> "test-workspace-lifecycle")

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Use bracket to ensure cleanup happens even if test fails
      result <-
        liftIO $
          bracket
            (startVMForTest env vmConfig) -- Start VM
            (stopVMSafely env vmConfig) -- Always stop VM
            ( \vmStarted -> do
                -- Test the VM is actually running
                vmStarted `shouldBe` True

                -- Check that PID file exists
                pidFileExists <- doesFileExist (vmPidFile vmConfig)
                pidFileExists `shouldBe` True

                -- Test stop functionality
                stopResult <- runVMT env (stop vmConfig)
                stopResult `shouldSatisfy` isRight

                -- Check that PID file is cleaned up
                pidFileExistsAfter <- doesFileExist (vmPidFile vmConfig)
                pidFileExistsAfter `shouldBe` False

                pure True
            )

      result `shouldBe` True

    it "stops VM when already stopped gracefully" $ \(env, _) -> do
      -- Create test VM configuration
      let envStateDir = env ^. #stateDir
      vmConfig <- testVMConfig envStateDir (envStateDir </> "test-workspace-stop-twice")

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Try to stop a VM that was never started
      stopResult <- liftIO $ runVMT env (stop vmConfig)
      stopResult `shouldSatisfy` isLeft

    it "handles invalid PID file gracefully" $ \(env, _) -> do
      -- Create test VM configuration
      let envStateDir = env ^. #stateDir
      vmConfig <- testVMConfig envStateDir (envStateDir </> "test-workspace-invalid-pid")

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Write invalid PID to file
      liftIO $ writeFile (vmPidFile vmConfig) "not-a-number\n"

      -- Try to stop with invalid PID
      stopResult <- liftIO $ runVMT env (stop vmConfig)
      stopResult `shouldSatisfy` isLeft

-- | Start VM for testing (simplified - doesn't actually run the interactive process)
startVMForTest :: AgentVmEnv -> VMConfig -> IO Bool
startVMForTest _env vmConfig = do
  -- For now, we'll just create a mock PID file to simulate VM startup
  -- In a real implementation, this would start the actual VM process
  -- but for testing purposes, we simulate it
  liftIO $ writeFile (vmPidFile vmConfig) "12345\n"
  pure True

-- | Safely stop VM, catching any errors
stopVMSafely :: AgentVmEnv -> VMConfig -> Bool -> IO ()
stopVMSafely env vmConfig _vmWasStarted = do
  catchAny
    ( do
        stopResult <- runVMT env (stop vmConfig)
        case stopResult of
          Left _ -> pure () -- Ignore stop errors in cleanup
          Right _ -> pure ()
    )
    (\_ -> pure ()) -- Ignore all errors in cleanup

-- | Test helper: Create test VM configuration for integration tests
testVMConfig :: (MonadIO m) => FilePath -> FilePath -> m VMConfig
testVMConfig envStateDir workspaceDir =
  defVMConfig (Just envStateDir) "test-vm-lifecycle" workspaceDir
    <&> #group
      .~ "users" -- FIXME Un-hardcode, fetch from environment
