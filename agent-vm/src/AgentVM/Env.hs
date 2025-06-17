{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Environment module for AgentVM operations
module AgentVM.Env
  ( AgentVmEnv (..),
    runVM,
  )
where

import AgentVM.Log (AgentVmTrace)
import Plow.Logging (IOTracer)
import Protolude

-- | Environment for agent VM operations
{-# ANN AgentVmEnv "HLint: ignore Use newtype instead of data" #-}

data AgentVmEnv = AgentVmEnv
  { tracer :: IOTracer AgentVmTrace
  -- Add more fields as needed
  }
  deriving (Generic)

-- | Run a VM operation in the AgentVmEnv environment
runVM :: AgentVmEnv -> ReaderT AgentVmEnv m a -> m a
runVM = flip runReaderT
