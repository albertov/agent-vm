{- This executable is for tests that need to be able to create
 - VMs
 -}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import qualified AgentVM.NixSpec as NixSpec
import qualified AgentVM.ResetDestroySpec as ResetDestroySpec
import qualified AgentVM.ShellEscapeKeySpec as ShellEscapeKeySpec
import AgentVM.TestUtils (withTestEnv)
import qualified AgentVM.VMLifecycleSpec as VMLifecycleSpec
import Protolude
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Agent VM Integration Tests" $ do
  NixSpec.spec
  VMLifecycleSpec.spec
  ResetDestroySpec.spec
  ShellEscapeKeySpec.spec
  describe "VM Lifecycle" $ around withTestEnv $ do
    it "can start a VM" $ \(_env, _) -> do
      pending -- TODO: Implement start command
    it "can stop a VM" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement stop command
    it "can show VM status" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement status command
    it "can destroy a VM" $ \(_env, _tracesRef) ->
      pending -- Implemented in ResetDestroySpec
    it "can list all VMs" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement list command
    it "can reset VM disk" $ \(_env, _tracesRef) ->
      pending -- Implemented in ResetDestroySpec
    it "can update VM configuration" $ \(_env, _tracesRef) ->
      pending -- TODO: Verify update command works end-to-end
    it "can connect to VM shell" $ \(_env, _tracesRef) ->
      pending -- TODO: Verify shell command works end-to-end
    it "completes full create-start-stop-destroy cycle" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "prevents duplicate VM creation for same name" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
  describe "VM State Management" $ around withTestEnv $ do
    it "persists VM configuration across operations" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "tracks VM process PIDs accurately" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "validates VM configuration before operations" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "handles VM name inference from git repository" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
  describe "CLI Interface" $ around withTestEnv $ do
    it "parses command line arguments correctly" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "supports all documented commands" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "handles invalid commands gracefully" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "provides helpful error messages" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "supports verbose and debug logging" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
    it "supports custom state directory" $ \(_env, _tracesRef) ->
      pending -- TODO: Implement
