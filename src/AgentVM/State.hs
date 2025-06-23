{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | VM state management with STM
module AgentVM.State
  ( VMRegistry (..),
    VMInfo (..),
    newVMRegistry,
    withVMLock,
    registerVM,
    lookupVM,
    unregisterVM,
    allocatePort,
    releasePort,
    isPortAvailable,
  )
where

import AgentVM.Types (BranchName, VM, VMError (PortAllocationFailed), vmId, vmIdBranch)
import Control.Concurrent.STM (TVar, modifyTVar', newTVar, readTVar, retry, writeTVar)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Network.Socket (SockAddr (SockAddrInet), SocketType (Stream), bind, close, defaultProtocol, socket)
import qualified Network.Socket as Socket
import Protolude (Bool (False, True), Either (Left, Right), IO, Int, Maybe (Just, Nothing), STM, atomically, fromIntegral, otherwise, return, when, ($), (+), (.), (<$>), (<*>), (=<<), (>))
import UnliftIO.Exception (bracket, finally, tryAny)

-- | Thread-safe VM registry
data VMRegistry = VMRegistry
  { vmMap :: TVar (Map.Map BranchName VMInfo),
    vmLocks :: TVar (Set.Set BranchName),
    vmPorts :: TVar (Set.Set Int)
  }

-- | Runtime VM information
data VMInfo = forall s. VMInfo (VM s)

-- | Create a new VM registry
newVMRegistry :: IO VMRegistry
newVMRegistry =
  atomically $
    VMRegistry
      <$> newTVar Map.empty
      <*> newTVar Set.empty
      <*> newTVar Set.empty

-- | Acquire exclusive lock on a branch
withVMLock :: VMRegistry -> BranchName -> IO a -> IO a
withVMLock registry branch action = do
  atomically $ do
    locks <- readTVar (vmLocks registry)
    when (branch `Set.member` locks) retry
    writeTVar (vmLocks registry) (Set.insert branch locks)

  finally action $
    atomically $
      modifyTVar' (vmLocks registry) (Set.delete branch)

-- | Register a new VM
registerVM :: VMRegistry -> VM s -> STM ()
registerVM registry vm = do
  vms <- readTVar (vmMap registry)
  case Map.lookup (vmIdBranch $ vmId vm) vms of
    Just _ -> retry -- TODO: proper error handling for VM already exists
    Nothing ->
      writeTVar
        (vmMap registry)
        (Map.insert (vmIdBranch $ vmId vm) (VMInfo vm) vms)

-- | Find a VM by branch name
lookupVM :: VMRegistry -> BranchName -> STM (Maybe VMInfo)
lookupVM registry branch = Map.lookup branch <$> readTVar (vmMap registry)

-- | Remove a VM from registry
unregisterVM :: VMRegistry -> BranchName -> STM ()
unregisterVM registry branch =
  modifyTVar' (vmMap registry) (Map.delete branch)

-- | Check if a port is available on the system
isPortAvailable :: Int -> IO Bool
isPortAvailable port = do
  result <-
    tryAny $
      bracket
        (socket Socket.AF_INET Stream defaultProtocol)
        close
        ( \sock -> do
            Socket.setSocketOption sock Socket.ReuseAddr 1
            bind sock (SockAddrInet (fromIntegral port) 0)
        )
  return $ case result of
    Left _ -> False -- Port is in use
    Right _ -> True -- Port is available

-- | Allocate a free port
allocatePort :: VMRegistry -> Int -> IO (Either VMError Int)
allocatePort registry startPort = do
  -- First, find a port that's not in our registry
  candidatePort <- atomically $ do
    ports <- readTVar (vmPorts registry)
    let findFree p
          | p > startPort + 100 = Left PortAllocationFailed
          | p `Set.member` ports = findFree (p + 1)
          | otherwise = Right p
    return $ findFree startPort

  -- Then check if it's actually available on the system
  case candidatePort of
    Left err -> return (Left err)
    Right port -> do
      available <- isPortAvailable port
      if available
        then do
          -- Reserve it in our registry
          atomically $ writeTVar (vmPorts registry) . Set.insert port =<< readTVar (vmPorts registry)
          return (Right port)
        else
          -- Try the next port
          allocatePort registry (port + 1)

-- | Release a port
releasePort :: VMRegistry -> Int -> STM ()
releasePort registry port =
  modifyTVar' (vmPorts registry) (Set.delete port)
