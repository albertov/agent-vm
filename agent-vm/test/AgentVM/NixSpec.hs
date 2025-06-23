{-# LANGUAGE OverloadedStrings #-}

module AgentVM.NixSpec (spec) where

import AgentVM.Log (AgentVmTrace)
import AgentVM.Nix (buildVMConfig)
import AgentVM.Types (BranchName (BranchName), VMError (..))
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer))
import Protolude
import Test.Hspec (Spec, describe, it, pending, shouldBe, shouldReturn, shouldSatisfy)
import UnliftIO.Directory (createDirectoryIfMissing, withCurrentDirectory)
import UnliftIO.Temporary (withSystemTempDirectory)

-- | Test tracer that discards all logs
testTracer :: IOTracer AgentVmTrace
testTracer = IOTracer $ Tracer $ \_ -> return ()

spec :: Spec
spec = describe "AgentVM.Nix" $ do
  describe "buildVMConfig" $ do
    it "calls nix build with correct arguments" $ do
      pending -- Need to mock subprocess calls
    it "returns store path on successful build" $ do
      pending -- Need to mock subprocess calls
    it "handles build failures gracefully" $ do
      pending -- Need to mock subprocess calls
