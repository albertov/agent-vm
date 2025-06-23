-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig
  , runVMScript
  ) where

import AgentVM.Types
import AgentVM.Log
import System.Process.Typed
import Data.Text (Text)

-- | Build VM configuration using Nix
buildVMConfig :: LogAction IO AgentVmTrace -> BranchName -> FilePath -> IO (Either VMError FilePath)
buildVMConfig = error "buildVMConfig has not been implemented yet"

-- | Run Nix-generated VM script
runVMScript :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
runVMScript = error "runVMScript has not been implemented yet"
