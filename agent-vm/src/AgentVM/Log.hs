{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Logging infrastructure for agent-vm
module AgentVM.Log
  ( AgentVmTrace(..)
  , LogAction(..)
  , Severity(..)
  , traceToMessage
  , traceSeverity
  , renderTrace
  , vmLogger
  , (<&)
  ) where

import Protolude (Text, Generic, IO, show, FilePath, Show, Eq, Ord, ($), (<>), Int)

import AgentVM.Types (BranchName, unBranchName, VMConfig)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ANSI (Color(Red, Green, Yellow, Cyan), ColorIntensity(Dull, Vivid), ConsoleLayer(Foreground), SGR(SetColor, Reset), setSGR)
import System.IO (stderr)

-- | LogAction type for plow-log compatibility
newtype LogAction m a = LogAction { unLogAction :: a -> m () }

-- | Convenience operator for logging
(<&) :: LogAction m a -> a -> m ()
(<&) = unLogAction
infixr 1 <&

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
    VMCreated BranchName VMConfig
  | VMStarting BranchName
  | VMStarted BranchName Int
  | VMStopping BranchName
  | VMStopped BranchName
  | VMDestroyed BranchName
  | VMFailed BranchName Text
  -- Process Management
  | ProcessSpawned Text [Text]
  | ProcessExited Text Int
  | ProcessOutput Text Text
  | ProcessError Text Text
  -- SSH Operations
  | SSHKeyGenerated FilePath
  | SSHConnecting Text Int
  | SSHConnected Text Int
  | SSHCommandExecuted Text
  | SSHFailed Text
  -- Nix Operations
  | NixBuildStarted Text
  | NixBuildProgress Text
  | NixBuildCompleted FilePath
  | NixBuildFailed Text
  -- Network Operations
  | PortScanning Int
  | PortAllocated Int
  | PortReleased Int
  -- Workspace Operations
  | WorkspaceCreated FilePath
  | WorkspaceCloned Text FilePath
  | WorkspaceSynced FilePath
  | WorkspaceRemoved FilePath
  -- Agent Service
  | AgentServiceStarting BranchName
  | AgentServiceHealthy BranchName Text
  | AgentServiceFailed BranchName Text
  deriving (Show, Eq, Generic)

-- | Convert trace to message (full implementation)
traceToMessage :: AgentVmTrace -> Text
traceToMessage = renderTrace

-- | Determine severity from trace type
traceSeverity :: AgentVmTrace -> Severity
traceSeverity = \case
  VMFailed {} -> Error
  ProcessError {} -> Error
  SSHFailed {} -> Error
  AgentVM.Log.NixBuildFailed {} -> Error
  AgentServiceFailed {} -> Error

  VMCreated {} -> Info
  VMStarted {} -> Info
  VMStopped {} -> Info
  SSHConnected {} -> Info
  NixBuildCompleted {} -> Info
  PortAllocated {} -> Info
  WorkspaceCreated {} -> Info
  AgentServiceHealthy {} -> Info

  _ -> Debug

-- | Render trace as formatted text
renderTrace :: AgentVmTrace -> Text
renderTrace = \case
  VMCreated b _ -> "🆕 Created VM for branch " <> unBranchName b
  VMStarting b -> "⏳ Starting VM for " <> unBranchName b
  VMStarted b p -> "🚀 Started VM for " <> unBranchName b <> " (PID: " <> T.pack (show p) <> ")"
  VMStopping b -> "⏹️  Stopping VM for " <> unBranchName b
  VMStopped b -> "🛑 Stopped VM for " <> unBranchName b
  VMDestroyed b -> "🗑️  Destroyed VM for " <> unBranchName b
  VMFailed b r -> "❌ VM failed for " <> unBranchName b <> ": " <> r

  ProcessSpawned c a -> "🔧 Spawned process: " <> c <> " " <> T.unwords a
  ProcessExited c e -> "📤 Process exited: " <> c <> " (code: " <> T.pack (show e) <> ")"
  ProcessOutput c o -> "📝 Process output from " <> c <> ": " <> T.take 80 o
  ProcessError c e -> "❌ Process error from " <> c <> ": " <> e

  SSHKeyGenerated p -> "🔑 Generated SSH key: " <> T.pack p
  SSHConnecting h p -> "🔗 Connecting to SSH " <> h <> ":" <> T.pack (show p)
  SSHConnected h p -> "✅ SSH connected to " <> h <> ":" <> T.pack (show p)
  SSHCommandExecuted c -> "⚡ SSH command executed: " <> c
  SSHFailed r -> "❌ SSH failed: " <> r

  NixBuildStarted f -> "🔨 Building " <> f
  NixBuildProgress m -> "📊 Build progress: " <> m
  NixBuildCompleted p -> "✅ Built " <> T.pack p
  AgentVM.Log.NixBuildFailed e -> "❌ Build failed: " <> e

  PortScanning p -> "🔍 Scanning ports from " <> T.pack (show p)
  PortAllocated p -> "🔌 Allocated port " <> T.pack (show p)
  PortReleased p -> "🔓 Released port " <> T.pack (show p)

  WorkspaceCreated p -> "📁 Created workspace: " <> T.pack p
  WorkspaceCloned o d -> "📋 Cloned " <> o <> " to " <> T.pack d
  WorkspaceSynced p -> "🔄 Synced workspace: " <> T.pack p
  WorkspaceRemoved p -> "🗑️  Removed workspace: " <> T.pack p

  AgentServiceStarting b -> "🚀 Starting agent service for " <> unBranchName b
  AgentServiceHealthy b u -> "💚 Agent healthy on " <> unBranchName b <> " (up " <> u <> ")"
  AgentServiceFailed b e -> "❌ Agent failed on " <> unBranchName b <> ": " <> e

-- | Set color based on severity
setSeverityColor :: Severity -> IO ()
setSeverityColor severity =
  setSGR $ case severity of
    Debug -> [SetColor Foreground Dull Cyan]
    Info -> [SetColor Foreground Vivid Green]
    Warning -> [SetColor Foreground Vivid Yellow]
    Error -> [SetColor Foreground Vivid Red]

-- | Create a logger that outputs to stdout with colors
vmLogger :: LogAction IO AgentVmTrace
vmLogger = LogAction $ \traceEvent -> do
  let severity = traceSeverity traceEvent
      traceMessage = renderTrace traceEvent
  setSeverityColor severity
  T.hPutStr stderr $ "[" <> T.pack (show severity) <> "] "
  setSGR [Reset]
  T.hPutStrLn stderr traceMessage
