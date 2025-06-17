{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Core type definitions for agent-vm
module AgentVM.Types
  ( -- * VM States
    VMState (..),
    BranchName (..),
    VMId (..),
    VMConfig (..),
    VM (..),
    VMStateData (..),
    VMOp (..),
    VMError (..),
    VMHandle (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Kind (Type)
import Data.Time (UTCTime)
import Protolude (Eq, FilePath, Generic, Int, Ord, Show, Text)
import System.Process.Typed (Process)
-- import UnliftIO (SomeException)

-- | VM states as type-level values
data VMState = Stopped | Starting | Running | Stopping | Failed

-- | Branch name newtype for type safety
newtype BranchName = BranchName {unBranchName :: Text}
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | VM identifier
data VMId = VMId
  { vmIdBranch :: BranchName,
    vmIdHost :: Text
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | VM configuration
data VMConfig = VMConfig
  { vmConfigHost :: Text,
    vmConfigPort :: Int,
    vmConfigSshPort :: Int,
    vmConfigMemory :: Int,
    vmConfigCores :: Int,
    vmConfigWorkspace :: FilePath,
    vmConfigNixPath :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type-safe VM with phantom state
data VM (s :: VMState) = VM
  { vmId :: VMId,
    vmConfig :: VMConfig,
    vmCreatedAt :: UTCTime,
    vmStateData :: VMStateData s
  }

-- | State-specific data
data VMStateData (s :: VMState) where
  StoppedData :: VMStateData 'Stopped
  StartingData :: {startingPid :: Int} -> VMStateData 'Starting
  RunningData :: {runningPid :: Int, runningProcess :: Process () () ()} -> VMStateData 'Running
  StoppingData :: {stoppingPid :: Int} -> VMStateData 'Stopping
  FailedData :: {failureReason :: Text} -> VMStateData 'Failed

deriving instance Show (VMStateData s)

-- | VM operations GADT for type-safe transitions
data VMOp :: VMState -> VMState -> Type -> Type where
  Create :: VMConfig -> VMOp 'Stopped 'Stopped VMId
  Start :: VMOp 'Stopped 'Running (Process () () ())
  Stop :: VMOp 'Running 'Stopped ()
  ForceStop :: VMOp 'Running 'Stopped ()
  MarkFailed :: Text -> VMOp s 'Failed ()

-- | Handle to a running VM
data VMHandle = VMHandle
  { vmHandleId :: VMId,
    vmHandleConfig :: VMConfig,
    vmHandlePid :: Int
  }
  deriving (Eq, Show, Generic)

-- | Errors that can occur during VM operations
data VMError
  = VMAlreadyExists BranchName
  | VMNotFound BranchName
  | VMInvalidState Text
  | VMStartupTimeout
  | VMShutdownTimeout
  | SSHConnectionFailed Text
  | NixBuildFailed Text
  | PortAllocationFailed
  | WorkspaceError Text
  -- TODO: So the runVMT version can catch
  -- unhandled exceptions with tryAny
  -- | UnhandledException SomeException
  deriving (Eq, Show)
