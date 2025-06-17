{-# LANGUAGE DeriveGeneric #-}

-- | Logging infrastructure for agent-vm
module AgentVM.Log
  ( AgentVmTrace(..)
  , LogAction(..)
  , Severity(..)
  , traceToMessage
  , traceSeverity
  , renderTrace
  , vmLogger
  ) where

import AgentVM.Types
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | LogAction type for plow-log compatibility
newtype LogAction m a = LogAction { unLogAction :: a -> m () }

-- | Severity levels for logging
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord, Generic)

-- | All possible log events in the system
data AgentVmTrace
  = -- VM Lifecycle
    VMCreated { branch :: BranchName, config :: VMConfig }
  | VMStarting { branch :: BranchName }
  | VMStarted { branch :: BranchName, pid :: Int }
  | VMStopping { branch :: BranchName }
  | VMStopped { branch :: BranchName }
  | VMDestroyed { branch :: BranchName }
  | VMFailed { branch :: BranchName, reason :: Text }
  -- Process Management
  | ProcessSpawned { cmd :: Text, args :: [Text] }
  | ProcessExited { cmd :: Text, exitCode :: Int }
  | ProcessOutput { cmd :: Text, output :: Text }
  | ProcessError { cmd :: Text, errorMsg :: Text }
  -- SSH Operations
  | SSHKeyGenerated { keyPath :: FilePath }
  | SSHConnecting { host :: Text, port :: Int }
  | SSHConnected { host :: Text, port :: Int }
  | SSHCommandExecuted { cmd :: Text }
  | SSHFailed { reason :: Text }
  -- Nix Operations
  | NixBuildStarted { flakeRef :: Text }
  | NixBuildProgress { message :: Text }
  | NixBuildCompleted { storePath :: FilePath }
  | NixBuildFailed { errorMsg :: Text }
  deriving (Show, Eq, Generic)

-- | Convert trace to message (stub)
traceToMessage :: AgentVmTrace -> Text
traceToMessage = error "traceToMessage has not been implemented yet"

-- | Determine severity from trace type
traceSeverity :: AgentVmTrace -> Severity
traceSeverity = error "traceSeverity has not been implemented yet"

-- | Render trace as formatted text
renderTrace :: AgentVmTrace -> Text
renderTrace = error "renderTrace has not been implemented yet"

-- | Create a logger that outputs to stdout with colors
vmLogger :: LogAction IO AgentVmTrace
vmLogger = error "vmLogger has not been implemented yet"
