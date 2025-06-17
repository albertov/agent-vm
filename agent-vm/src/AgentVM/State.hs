{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

-- | VM state management with STM
module AgentVM.State
  ( VMRegistry(..)
  , VMInfo(..)
  , newVMRegistry
  , withVMLock
  , registerVM
  , lookupVM
  , unregisterVM
  , allocatePort
  , releasePort
  ) where

import Protolude (IO, STM, atomically, when, Int, Maybe(Just, Nothing), Either(Left, Right), return, otherwise, (+), (>), ($), (<$>), (<*>))

import AgentVM.Types (BranchName, VM, vmId, vmIdBranch, VMError(PortAllocationFailed))
import Control.Concurrent.STM (TVar, newTVar, readTVar, writeTVar, modifyTVar', retry)
import UnliftIO.Exception (finally, throwString)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

-- | Thread-safe VM registry
data VMRegistry = VMRegistry
  { vmMap :: TVar (Map.Map BranchName VMInfo)
  , vmLocks :: TVar (Set.Set BranchName)
  , vmPorts :: TVar (Set.Set Int)
  }

-- | Runtime VM information
data VMInfo = forall s. VMInfo (VM s)

-- | Create a new VM registry
newVMRegistry :: IO VMRegistry
newVMRegistry = atomically $ VMRegistry
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

  finally action $ atomically $
    modifyTVar' (vmLocks registry) (Set.delete branch)

-- | Register a new VM
registerVM :: VMRegistry -> VM s -> STM ()
registerVM registry vm = do
  vms <- readTVar (vmMap registry)
  case Map.lookup (vmIdBranch $ vmId vm) vms of
    Just _ -> retry -- TODO: proper error handling for VM already exists
    Nothing -> writeTVar (vmMap registry)
                (Map.insert (vmIdBranch $ vmId vm) (VMInfo vm) vms)

-- | Find a VM by branch name
lookupVM :: VMRegistry -> BranchName -> STM (Maybe VMInfo)
lookupVM registry branch = Map.lookup branch <$> readTVar (vmMap registry)

-- | Remove a VM from registry
unregisterVM :: VMRegistry -> BranchName -> STM ()
unregisterVM registry branch =
  modifyTVar' (vmMap registry) (Map.delete branch)

-- | Allocate a free port
allocatePort :: VMRegistry -> Int -> STM (Either VMError Int)
allocatePort registry startPort = do
  ports <- readTVar (vmPorts registry)
  let findFree p
        | p > startPort + 100 = Left PortAllocationFailed
        | p `Set.member` ports = findFree (p + 1)
        | otherwise = Right p
  case findFree startPort of
    Right port -> do
      writeTVar (vmPorts registry) (Set.insert port ports)
      return (Right port)
    Left err -> return (Left err)

-- | Release a port
releasePort :: VMRegistry -> Int -> STM ()
releasePort registry port =
  modifyTVar' (vmPorts registry) (Set.delete port)
