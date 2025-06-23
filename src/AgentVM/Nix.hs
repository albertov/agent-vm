{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig,
    runVMScript,
  )
where

import AgentVM.Types (BranchName, VMError)
import Protolude (Either, FilePath, notImplemented)
import System.Process.Typed (Process)

-- | Build VM configuration using Nix
buildVMConfig ::
  BranchName ->
  FilePath ->
  m (Either VMError FilePath)
buildVMConfig _ _ = notImplemented

-- | Run Nix-generated VM script
runVMScript ::
  FilePath ->
  m (Process () () ())
runVMScript _ = notImplemented
