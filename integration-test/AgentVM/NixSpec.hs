{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module AgentVM.NixSpec (spec) where

import AgentVM (MonadVM (..), runVMT)
import AgentVM.TestUtils (withTestEnv)
import AgentVM.Types (vmGCRoot, vmNixFile)
import Data.Generics.Labels ()
import Lens.Micro
import Protolude
import System.Directory (doesDirectoryExist, doesFileExist)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = describe "MonadVM Integration Tests" $ around withTestEnv $ do
  describe "create" $ do
    it "creates VM directory structure with all required files" $ \(env, _) -> do
      -- Create test VM configuration
      let vmConfig = env ^. #vmConfig
          vmDir = vmConfig ^. #stateDir

      -- Run buildVMImage
      result <- liftIO $ runVMT env (create vmConfig)

      -- Should succeed
      result `shouldSatisfy` isRight

      -- Directory should exist
      dirExists <- liftIO $ doesDirectoryExist vmDir
      dirExists `shouldBe` True

      -- vm.nix file should exist
      nixFileExists <- liftIO $ doesFileExist (vmNixFile vmConfig)
      nixFileExists `shouldBe` True

      -- gc-root symlink should exist
      gcRootExists <- liftIO $ doesDirectoryExist (vmGCRoot vmConfig)
      gcRootExists `shouldBe` True

    it "fails when directory already exists" $ \(env, _) -> do
      let vmConfig = env ^. #vmConfig

      -- First call should succeed
      result1 <- liftIO $ runVMT env (create vmConfig)
      result1 `shouldSatisfy` isRight

      -- Second call should fail
      result2 <- liftIO $ runVMT env (create vmConfig)
      result2 `shouldSatisfy` isLeft
