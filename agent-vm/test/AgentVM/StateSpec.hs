-- | Tests for VM state management
module AgentVM.StateSpec (spec) where

import AgentVM.State (VMRegistry (..), allocatePort, newVMRegistry, releasePort)
import Control.Concurrent.STM (TVar, atomically, readTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Protolude
import Test.Hspec (Spec, describe, expectationFailure, it, pending, shouldBe)

spec :: Spec
spec = describe "AgentVM.State" $ do
  describe "VM Registry" $ do
    it "creates a new registry" $ do
      registry <- newVMRegistry

      -- Check that all components are initialized empty
      vmMapEmpty <- readTVarIO (vmMap registry)
      Map.null vmMapEmpty `shouldBe` True

      vmLocksEmpty <- readTVarIO (vmLocks registry)
      Set.null vmLocksEmpty `shouldBe` True

      vmPortsEmpty <- readTVarIO (vmPorts registry)
      Set.null vmPortsEmpty `shouldBe` True

    it "registers a VM" $ do
      pending

    it "prevents duplicate VM registration" $ do
      pending

    it "looks up VMs by branch name" $ do
      pending

    it "unregisters VMs" $ do
      pending

  describe "Port allocation" $ do
    it "allocates free ports" $ do
      registry <- newVMRegistry

      -- Allocate a port starting from 8000
      result <- atomically $ allocatePort registry 8000

      case result of
        Right port -> port `shouldBe` 8000
        Left err -> expectationFailure $ "Port allocation failed: " <> show err

    it "prevents port conflicts" $ do
      registry <- newVMRegistry

      -- Allocate first port
      result1 <- atomically $ allocatePort registry 8000
      result1 `shouldBe` Right 8000

      -- Try to allocate again from same starting port
      result2 <- atomically $ allocatePort registry 8000
      result2 `shouldBe` Right 8001 -- Should get next available port
    it "releases ports" $ do
      registry <- newVMRegistry

      -- Allocate a port
      result1 <- atomically $ allocatePort registry 8000
      result1 `shouldBe` Right 8000

      -- Release the port
      atomically $ releasePort registry 8000

      -- Should be able to allocate the same port again
      result2 <- atomically $ allocatePort registry 8000
      result2 `shouldBe` Right 8000

  describe "VM locking" $ do
    it "acquires exclusive locks" $ do
      pending

    it "releases locks on completion" $ do
      pending

    it "handles concurrent lock requests" $ do
      pending
