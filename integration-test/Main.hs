{- This executable is for tests that need to be able to create
 - VMs
 -}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import AgentVM (MonadVM (..), loadVMConfig, runVMT)
import AgentVM.Git (generateDefaultName, gitInit)
import qualified AgentVM.NixSpec as NixSpec
import qualified AgentVM.ResetDestroySpec as ResetDestroySpec
import AgentVM.TestUtils (withTestEnv)
import AgentVM.Types (VMConfig (..), VMError (..), VMState (..), vmConfigFile)
import qualified AgentVM.VMLifecycleSpec as VMLifecycleSpec
import Data.Generics.Labels ()
import Lens.Micro ((^.))
import Protolude
import qualified Shelly as Sh
import System.Directory (createDirectoryIfMissing)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Agent VM Integration Tests" $ do
  NixSpec.spec
  VMLifecycleSpec.spec
  ResetDestroySpec.spec
  describe "VM Command Tests" $ around withTestEnv $ do
    it "can show VM status" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      -- Create VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Check status
      statusResult <- liftIO $ runVMT env (status vmConfig)
      statusResult `shouldSatisfy` isRight
      case statusResult of
        Right vmState -> vmState `shouldBe` Stopped
        Left _ -> panic "Status check failed"

    it "can update VM configuration" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      -- Create VM first
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Update with different memory size
      let updatedConfig = vmConfig {memorySize = 8}
      updateResult <- liftIO $ runVMT env (update updatedConfig)
      updateResult `shouldSatisfy` isRight

      -- Verify config file was updated
      reloadedConfig <- liftIO $ loadVMConfig (vmConfigFile vmConfig)
      case reloadedConfig of
        Just cfg -> memorySize cfg `shouldBe` 8
        Nothing -> panic "Failed to reload config"

    it "prevents duplicate VM creation for same name" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      -- Create VM first
      createResult1 <- liftIO $ runVMT env (create vmConfig)
      createResult1 `shouldSatisfy` isRight

      -- Try to create again with same name
      createResult2 <- liftIO $ runVMT env (create vmConfig)
      createResult2 `shouldSatisfy` isLeft
      case createResult2 of
        Left (WorkspaceError _) -> pure () -- Expected error
        _ -> panic "Expected WorkspaceError for duplicate creation"

  describe "VM State Management" $ around withTestEnv $ do
    it "persists VM configuration across operations" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig
      -- Create VM
      createResult <- liftIO $ runVMT env (create vmConfig)
      createResult `shouldSatisfy` isRight

      -- Reload config from disk
      reloadedConfig <- liftIO $ loadVMConfig (vmConfigFile vmConfig)
      reloadedConfig `shouldBe` Just vmConfig

    it "handles VM name inference from git repository" $ \(env, _) -> do
      -- Set up a git repository with specific repo name and branch
      let vmConfig = env ^. #vmConfig
          workspaceDir = vmConfig ^. #workspace

      -- Create workspace directory first
      liftIO $ createDirectoryIfMissing True workspaceDir

      -- Initialize git repository
      liftIO $ gitInit workspaceDir

      -- Set up a fake remote with a specific repository name
      liftIO $ do
        -- Create an initial file
        writeFile (workspaceDir <> "/README.md") "# Test Repository\n"

        Sh.shelly $ do
          Sh.run_ "git" ["-C", toS workspaceDir, "remote", "add", "origin", "https://github.com/test-user/my-awesome-repo.git"]
          -- Create and checkout a feature branch
          Sh.run_ "git" ["-C", toS workspaceDir, "checkout", "-b", "feature-branch"]
          -- Add and commit the file
          Sh.run_ "git" ["-C", toS workspaceDir, "add", "README.md"]
          Sh.run_ "git" ["-C", toS workspaceDir, "commit", "-m", "Initial commit"]

      -- Test name generation
      derivedName <- liftIO $ generateDefaultName workspaceDir
      derivedName `shouldBe` "my_awesome_repo-feature_branch"
