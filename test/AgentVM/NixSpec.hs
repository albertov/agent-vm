{-# LANGUAGE OverloadedStrings #-}

module AgentVM.NixSpec (spec) where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log (vmLogger)
import Protolude
import Test.Hspec (Spec, describe, it, pending)

-- | Test environment with tracer
_testEnv :: AgentVmEnv
_testEnv = AgentVmEnv {tracer = vmLogger, stateDir = "/tmp/test"}

spec :: Spec
spec = describe "AgentVM.Nix" $ do
  describe "buildVMConfig" $ do
    it "calls nix build with correct arguments" $ do
      pending -- Need to mock subprocess calls
    it "returns store path on successful build" $ do
      pending -- Need to mock subprocess calls
    it "handles build failures gracefully" $ do
      pending -- Need to mock subprocess calls
