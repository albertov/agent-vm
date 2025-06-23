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

import AgentVM.Types
import Control.Concurrent.STM
import Control.Exception (finally)
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- | Thread-safe VM registry
data VMRegistry = VMRegistry
  { vmMap :: TVar (Map BranchName VMInfo)
  , vmLocks :: TVar (Set BranchName)
  , vmPorts :: TVar (Set Int)
  }

-- | Runtime VM information
data VMInfo = forall s. VMInfo (VM s)

-- | Create a new VM registry
newVMRegistry :: IO VMRegistry
newVMRegistry = error "newVMRegistry has not been implemented yet"

-- | Acquire exclusive lock on a branch
withVMLock :: VMRegistry -> BranchName -> IO a -> IO a
withVMLock = error "withVMLock has not been implemented yet"

-- | Register a new VM
registerVM :: VMRegistry -> VM s -> STM ()
registerVM = error "registerVM has not been implemented yet"

-- | Find a VM by branch name
lookupVM :: VMRegistry -> BranchName -> STM (Maybe VMInfo)
lookupVM = error "lookupVM has not been implemented yet"

-- | Remove a VM from registry
unregisterVM :: VMRegistry -> BranchName -> STM ()
unregisterVM = error "unregisterVM has not been implemented yet"

-- | Allocate a free port
allocatePort :: VMRegistry -> Int -> STM (Either VMError Int)
allocatePort = error "allocatePort has not been implemented yet"

-- | Release a port
releasePort :: VMRegistry -> Int -> STM ()
releasePort = error "releasePort has not been implemented yet"
