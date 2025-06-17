{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Logging infrastructure for agent-vm
module AgentVM.Log
  ( AgentVmTrace (..),
    Severity (..),
    traceToMessage,
    traceSeverity,
    renderTrace,
    vmLogger,
    (<&),
  )
where

import AgentVM.Types (BranchName, VMConfig, unBranchName)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer), traceWith)
import Protolude (Eq, FilePath, Generic, Int, MonadIO, Ord, Show, Text, show, ($), (.), (<>))
import System.Console.ANSI (Color (Cyan, Green, Red, Yellow), ColorIntensity (Dull, Vivid), ConsoleLayer (Foreground), SGR (Reset, SetColor))
import qualified System.Console.ANSI as ANSI
import System.IO (Handle, stderr)
import UnliftIO (liftIO)

-- | MonadIO version of setSGR
setSGR :: (MonadIO m) => [SGR] -> m ()
setSGR = liftIO . ANSI.setSGR

-- | MonadIO version of Text.IO.hPutStr
hPutStr :: (MonadIO m) => Handle -> Text -> m ()
hPutStr h = liftIO . TIO.hPutStr h

-- | MonadIO version of Text.IO.hPutStrLn
hPutStrLn :: (MonadIO m) => Handle -> Text -> m ()
hPutStrLn h = liftIO . TIO.hPutStrLn h

-- | Convenience operator for logging using traceWith
(<&) :: Tracer m a -> a -> m ()
(<&) = traceWith

infixr 1 <&

-- | Severity levels for logging
data Severity
  = Debug
  | Info
  | Warning
  | Error
  deriving (Show, Eq, Ord, Generic)

-- | All possible log events in the system
-- FIXME: I want much more context in the constructors
-- here so we can debug better
data AgentVmTrace
  = -- VM Lifecycle
    VMCreated BranchName VMConfig
  | VMStarting BranchName
  | VMStarted BranchName Int
  | VMStopping BranchName
  | VMStopped BranchName
  | VMDestroyed BranchName
  | VMFailed BranchName Text
  | -- Process Management
    ProcessSpawned Text [Text]
  | ProcessExited Text Int
  | ProcessOutput Text Text
  | ProcessError Text Text
  | -- SSH Operations
    SSHKeyGenerated FilePath
  | SSHConnecting Text Int
  | SSHConnected Text Int
  | SSHCommandExecuted Text
  | SSHFailed Text
  | -- Nix Operations
    NixBuildStarted Text
  | NixBuildProgress Text
  | NixBuildCompleted FilePath
  | NixBuildFailed Text
  | -- Network Operations
    PortScanning Int
  | PortAllocated Int
  | PortReleased Int
  | -- Workspace Operations
    WorkspaceCreated FilePath
  | WorkspaceCloned Text FilePath
  | WorkspaceSynced FilePath
  | WorkspaceRemoved FilePath
  | -- Agent Service
    AgentServiceStarting BranchName
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
setSeverityColor :: (MonadIO m) => Severity -> m ()
setSeverityColor severity =
  setSGR $ case severity of
    Debug -> [SetColor Foreground Dull Cyan]
    Info -> [SetColor Foreground Vivid Green]
    Warning -> [SetColor Foreground Vivid Yellow]
    Error -> [SetColor Foreground Vivid Red]

-- | Create a logger that outputs to stderr with colors
vmLogger :: IOTracer AgentVmTrace
vmLogger = IOTracer $ Tracer $ \traceEvent -> do
  let severity = traceSeverity traceEvent
      traceMessage = renderTrace traceEvent
  setSeverityColor severity
  hPutStr stderr $ "[" <> T.pack (show severity) <> "] "
  setSGR [Reset]
  hPutStrLn stderr traceMessage
