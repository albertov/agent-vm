{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AgentVM.VMLifecycleSpec (spec) where

import AgentVM (MonadVM (..), runVMT)
import AgentVM.Env (AgentVmEnv)
import AgentVM.TestUtils (withGitWorkspaceTestEnv)
import AgentVM.Types (VMConfig (..), vmPidFile)
import AgentVM.VMCache (lookupVMByWorkspace)
import Data.Generics.Labels ()
import Lens.Micro
import Protolude hiding (bracket)
import System.Directory (doesFileExist)
import System.FilePath (takeDirectory)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)
import UnliftIO (catchAny)
import UnliftIO.Exception (bracket)

spec :: Spec
spec = describe "VM Lifecycle Integration Tests" $ do
  describe "VM Start/Stop with proper cleanup" $ do
    around withGitWorkspaceTestEnv $ it "can start and stop a VM with bracket pattern" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

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

    around withGitWorkspaceTestEnv $ it "stops VM when already stopped gracefully" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Try to stop a VM that was never started
      stopResult <- liftIO $ runVMT env (stop vmConfig)
      stopResult `shouldSatisfy` isLeft

    around withGitWorkspaceTestEnv $ it "handles invalid PID file gracefully" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Write invalid PID to file
      liftIO $ writeFile (vmPidFile vmConfig) "not-a-number\n"

      -- Try to stop with invalid PID
      stopResult <- liftIO $ runVMT env (stop vmConfig)
      stopResult `shouldSatisfy` isLeft

  describe "VM Cache functionality" $ do
    around withGitWorkspaceTestEnv $ it "should populate cache when creating a VM" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Check cache was populated
      let workspacePath = vmConfig ^. #workspace
          vmName = vmConfig ^. #name
          stateDir' = takeDirectory (vmConfig ^. #stateDir)
      cachedName <- liftIO $ lookupVMByWorkspace (Just stateDir') workspacePath
      cachedName `shouldBe` Just vmName

    around withGitWorkspaceTestEnv $ it "should remove from cache when destroying a VM" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Destroy the VM
      destroyResult <- liftIO $ runVMT env (destroy vmConfig)
      destroyResult `shouldSatisfy` isRight

      -- Check cache was cleared
      let workspacePath = vmConfig ^. #workspace
          stateDir' = takeDirectory (vmConfig ^. #stateDir)
      cachedName <- liftIO $ lookupVMByWorkspace (Just stateDir') workspacePath
      cachedName `shouldBe` Nothing

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
