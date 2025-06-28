{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AgentVM.ResetDestroySpec (spec) where

import AgentVM (MonadVM (..), runVMT)
import AgentVM.TestUtils (withTestEnv)
import AgentVM.Types (VMConfig (..), vmConfigFile, vmDiskImage)
import Data.Generics.Labels ()
import Lens.Micro
import Protolude
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "VM Reset and Destroy Integration Tests" $ around withTestEnv $ do
  describe "VM Reset" $ do
    it "resets VM by deleting disk but keeping configuration" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Create a fake disk image to simulate VM with data
      let diskPath = vmDiskImage vmConfig
      liftIO $ writeFile diskPath "fake disk data"

      -- Verify both config and disk exist
      configExists <- liftIO $ doesFileExist (vmConfigFile vmConfig)
      diskExists <- liftIO $ doesFileExist diskPath
      configExists `shouldBe` True
      diskExists `shouldBe` True

      -- Reset the VM
      resetResult <- liftIO $ runVMT env (reset vmConfig)
      resetResult `shouldSatisfy` isRight

      -- Verify config still exists but disk is gone
      configExistsAfter <- liftIO $ doesFileExist (vmConfigFile vmConfig)
      diskExistsAfter <- liftIO $ doesFileExist diskPath
      configExistsAfter `shouldBe` True
      diskExistsAfter `shouldBe` False

    it "handles reset when disk doesn't exist" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Verify disk doesn't exist (create doesn't actually create disk in tests)
      let diskPath = vmDiskImage vmConfig
      diskExists <- liftIO $ doesFileExist diskPath
      diskExists `shouldBe` False

      -- Reset should succeed even with no disk
      resetResult <- liftIO $ runVMT env (reset vmConfig)
      resetResult `shouldSatisfy` isRight

  describe "VM Destroy" $ do
    it "destroys VM by removing entire state directory" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create the VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Create some additional files to simulate VM state
      let stateDir' = stateDir vmConfig
      let diskPath = vmDiskImage vmConfig
      liftIO $ createDirectoryIfMissing True stateDir'
      liftIO $ writeFile diskPath "fake disk data"
      liftIO $ writeFile (stateDir' <> "/extra-file.txt") "extra data"

      -- Verify state directory exists
      stateDirExists <- liftIO $ doesDirectoryExist stateDir'
      stateDirExists `shouldBe` True

      -- Destroy the VM
      destroyResult <- liftIO $ runVMT env (destroy vmConfig)
      destroyResult `shouldSatisfy` isRight

      -- Verify entire state directory is gone
      stateDirExistsAfter <- liftIO $ doesDirectoryExist stateDir'
      stateDirExistsAfter `shouldBe` False

    it "handles destroy when state directory doesn't exist" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      let stateDir' = stateDir vmConfig

      -- Verify state directory doesn't exist
      stateDirExists <- liftIO $ doesDirectoryExist stateDir'
      stateDirExists `shouldBe` False

      -- Destroy should succeed even with no state directory
      destroyResult <- liftIO $ runVMT env (destroy vmConfig)
      destroyResult `shouldSatisfy` isRight

  describe "Reset vs Destroy Comparison" $ do
    it "demonstrates difference between reset and destroy" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- Create two identical VMs for comparison
      createResult1 <- liftIO $ runVMT env (create vmConfig)
      createResult1 `shouldSatisfy` isRight

      -- Setup state for both VMs
      let stateDir' = stateDir vmConfig
      let diskPath = vmDiskImage vmConfig
      let configPath = vmConfigFile vmConfig
      liftIO $ createDirectoryIfMissing True stateDir'
      liftIO $ writeFile diskPath "fake disk data"
      liftIO $ writeFile (stateDir' <> "/extra-file.txt") "extra data"

      -- Test reset: keeps config and state dir, removes disk
      resetResult <- liftIO $ runVMT env (reset vmConfig)
      resetResult `shouldSatisfy` isRight

      stateDirExistsAfterReset <- liftIO $ doesDirectoryExist stateDir'
      configExistsAfterReset <- liftIO $ doesFileExist configPath
      diskExistsAfterReset <- liftIO $ doesFileExist diskPath
      extraFileExistsAfterReset <- liftIO $ doesFileExist (stateDir' <> "/extra-file.txt")

      stateDirExistsAfterReset `shouldBe` True
      configExistsAfterReset `shouldBe` True
      diskExistsAfterReset `shouldBe` False
      extraFileExistsAfterReset `shouldBe` True

      -- Now test destroy: removes everything
      destroyResult <- liftIO $ runVMT env (destroy vmConfig)
      destroyResult `shouldSatisfy` isRight

      stateDirExistsAfterDestroy <- liftIO $ doesDirectoryExist stateDir'
      stateDirExistsAfterDestroy `shouldBe` False
