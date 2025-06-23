-- | Tests for VM state management
module AgentVM.StateSpec (spec) where

import AgentVM.State (VMRegistry (..), allocatePort, newVMRegistry, releasePort)
import Control.Concurrent.STM (atomically, readTVarIO)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Network.Socket (SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, listen, socket)
import qualified Network.Socket as Socket
import Protolude (Bool (True), Either (Left, Right), IO, show, ($), (/=), (<>), (>), (>=))
import Test.Hspec (Spec, describe, expectationFailure, it, pending, shouldBe, shouldSatisfy)

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

      -- Allocate a port starting from 10000 (less likely to be in use)
      result <- allocatePort registry 10000

      case result of
        Right port -> port `shouldSatisfy` (>= 10000)
        Left err -> expectationFailure $ "Port allocation failed: " <> show err

    it "prevents port conflicts" $ do
      registry <- newVMRegistry

      -- Allocate first port
      result1 <- allocatePort registry 10000
      case result1 of
        Right port1 -> do
          -- Try to allocate again from same starting port
          result2 <- allocatePort registry 10000
          case result2 of
            Right port2 -> do
              -- Should get a different port
              port2 `shouldSatisfy` (/= port1)
              port2 `shouldSatisfy` (> port1)
            Left err -> expectationFailure $ "Second allocation failed: " <> show err
        Left err -> expectationFailure $ "First allocation failed: " <> show err

    it "releases ports" $ do
      registry <- newVMRegistry

      -- Allocate a port
      result1 <- allocatePort registry 10000
      case result1 of
        Right port1 -> do
          -- Release the port
          atomically $ releasePort registry port1

          -- Should be able to allocate the same port again
          result2 <- allocatePort registry port1
          result2 `shouldBe` Right port1
        Left err -> expectationFailure $ "Port allocation failed: " <> show err

    it "checks actual system port availability" $ do
      registry <- newVMRegistry

      -- Create a socket to block port 20000 (high port less likely to be in use)
      sock <- socket Socket.AF_INET Stream defaultProtocol
      Socket.setSocketOption sock Socket.ReuseAddr 1
      bind sock (SockAddrInet 20000 0)
      Socket.listen sock 1 -- Actually listen on the port to ensure it's taken

      -- Try to allocate port 20000 - should skip it
      result <- allocatePort registry 20000
      case result of
        Right port -> do
          port `shouldSatisfy` (/= 20000) -- Should not be 20000
          port `shouldSatisfy` (>= 20001) -- Should be next available
        Left err -> expectationFailure $ "Port allocation failed: " <> show err

      -- Clean up
      close sock

  describe "VM locking" $ do
    it "acquires exclusive locks" $ do
      pending

    it "releases locks on completion" $ do
      pending

    it "handles concurrent lock requests" $ do
      pending
