{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Environment module for AgentVM operations
module AgentVM.Env
  ( AgentVmEnv (..),
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
