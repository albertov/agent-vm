{-# LANGUAGE OverloadedStrings #-}

module AgentVM.NixSpec (spec) where

import AgentVM.Log (AgentVmTrace, LogAction (LogAction))
import AgentVM.Nix (buildVMConfig)
import AgentVM.Types (BranchName (BranchName), VMError (..))
import Protolude
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

spec :: Spec
spec = describe "AgentVM.Nix" $ do
  describe "buildVMConfig" $ do
    it "calls nix build with correct arguments" $ do
      pending -- Need to mock subprocess calls
    it "returns store path on successful build" $ do
      pending -- Need to mock subprocess calls
    it "handles build failures gracefully" $ do
      pending -- Need to mock subprocess calls
