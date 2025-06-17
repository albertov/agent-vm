{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import System.IO.Temp
import Control.Exception (bracket)
import AgentVM
import AgentVM.Types

spec :: Spec
spec = describe "Agent VM Integration Tests" $ do

  describe "VM Lifecycle" $ do
    it "completes full create-start-stop-destroy cycle" $ do
      pending  -- TODO: Implement

    it "handles VM restart correctly" $ do
      pending  -- TODO: Implement

    it "manages multiple VM instances" $ do
      pending  -- TODO: Implement

  describe "State Management" $ do
    it "persists configuration across restarts" $ do
      pending  -- TODO: Implement

    it "manages SSH keys correctly" $ do
      pending  -- TODO: Implement

    it "handles concurrent state updates" $ do
      pending  -- TODO: Implement

  describe "Network and Connectivity" $ do
    it "establishes SSH connection after VM start" $ do
      pending  -- TODO: Implement

    it "forwards MCP proxy port correctly" $ do
      pending  -- TODO: Implement

    it "handles port conflicts gracefully" $ do
      pending  -- TODO: Implement

  -- Add all 50 integration tests as specified...

-- | Test helper: Create temporary state directory
withTempStateDir :: (FilePath -> IO a) -> IO a
withTempStateDir = withSystemTempDirectory "agent-vm-test"

-- | Test helper: Create test VM configuration
-- Pass it the path of the direcotry that withSytemTempDir returns
testVMConfig :: FilePath -> BranchName -> VMConfig
testVMConfig vmConfigWorkspace branch = VMConfig
  { vmConfigHost = "localhost"
  , vmConfigPort = 8000
  , vmConfigSshPort = 2222
  , vmConfigMemory = 4096
  , vmConfigCores = 4
  , vmConfigNixPath = "vm-config.nix"
  , vmConfigWorkspace
  }
