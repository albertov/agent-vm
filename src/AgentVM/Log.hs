{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Logging infrastructure for agent-vm
module AgentVM.Log
  ( AgentVmTrace (..),
    MonadTrace (..),
    LogLevel (..),
    traceToMessage,
    renderTrace,
    renderLogLevel,
    renderTracedMessage,
    traceLevel,
    vmLogger,
    vmLevelLogger,
    createLogContext,
  )
where

import AgentVM.Types hiding (VMError (..))
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import Plow.Logging
import Protolude hiding (trace)
import System.Console.ANSI

class (Monad m) => MonadTrace trace m | m -> trace where
  trace :: trace -> m ()

-- | Log level for filtering and importance tagging
data LogLevel
  = Trace
  | Debug
  | Info
  | Error
  | Critical
  deriving stock (Show, Eq, Read, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get the level as text
logLevelText :: LogLevel -> Text
logLevelText = \case
  Critical -> "CRITICAL"
  Error -> "ERROR"
  Info -> "INFO"
  Debug -> "DEBUG"
  Trace -> "TRACE"

-- | Render a LogLevel as formatted text with brackets
renderLogLevel :: LogLevel -> Text
renderLogLevel level = "[" <> logLevelText level <> "]"

-- | Render an AgentVmTrace with its level prefix
renderTracedMessage :: AgentVmTrace -> Text
renderTracedMessage traceEvent = renderLogLevel (traceLevel traceEvent) <> " " <> renderTrace traceEvent

-- | Get the appropriate LogLevel for an AgentVmTrace
traceLevel :: AgentVmTrace -> LogLevel
traceLevel traceEvent = case traceEvent of
  -- Critical events - system failures that need immediate attention
  VMFailed {} -> Critical
  -- Error events - failures that should be investigated
  ProcessTimeout {} -> Error
  ProcessSigKilled {} -> Error
  NixBuildFailed {} -> Error
  -- Info events - normal operational events
  VMCreated {} -> Info
  VMUpdated {} -> Info
  VMStarted {} -> Info
  VMStopped {} -> Info
  VMDestroyed {} -> Info
  VMConnectingShell {} -> Info
  NixBuildCompleted {} -> Info
  PortAllocated {} -> Info
  WorkspaceCreated {} -> Info
  -- Debug events - detailed operational info
  VMStarting {} -> Debug
  VMStopping {} -> Debug
  ProcessSpawned {} -> Debug
  ProcessExited {} -> Debug
  ProcessStopped {} -> Debug
  ProcessWaitingForExit {} -> Debug
  ProcessIOWaiting {} -> Debug
  ProcessGracefulStop {} -> Debug
  NixBuildStarted {} -> Debug
  NixBuildProgress {} -> Debug
  PortScanning {} -> Debug
  PortReleased {} -> Debug
  WorkspaceCloned {} -> Debug
  WorkspaceSynced {} -> Debug
  WorkspaceRemoved {} -> Debug
  MainInfo {} -> Info
  MainError {} -> Critical

-- | All possible log events in the system
-- FIXME: I want much more context in the constructors
-- here so we can debug better
data AgentVmTrace
  = -- VM Lifecycle
    VMCreated VMConfig
  | VMUpdated VMConfig
  | VMStarting VMConfig
  | VMStarted VMConfig Int
  | VMStopping VMConfig
  | VMStopped VMConfig
  | VMDestroyed VMConfig
  | VMFailed VMConfig Text
  | VMConnectingShell VMConfig
  | -- Process Management
    ProcessSpawned Text [Text]
  | ProcessExited Text Int
  | ProcessStopped Text -- Process gracefully stopped
  | ProcessSigKilled Text Int -- Process force killed with signal
  | ProcessTimeout Text Integer -- Process timed out after N microseconds
  | ProcessWaitingForExit Text -- Waiting for process to exit
  | ProcessIOWaiting Text -- Waiting for IO threads to complete
  | ProcessGracefulStop Text Integer -- Attempting graceful stop with timeout
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
    MainInfo Text
  | MainError Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert trace to message (full implementation)
traceToMessage :: AgentVmTrace -> Text
traceToMessage = renderTrace

-- | Render trace as formatted text
renderTrace :: AgentVmTrace -> Text
renderTrace = \case
  VMCreated c -> "🆕 Created VM at  " <> toS (stateDir c) <> " for " <> name c
  VMUpdated c -> "🔄 Updated VM configuration for " <> name c
  VMStarting c -> "⏳ Starting VM for " <> name c
  VMStarted c p -> "🚀 Started VM for " <> name c <> " (PID: " <> T.pack (show p) <> ")"
  VMStopping c -> "⏹️  Stopping VM for " <> name c
  VMStopped c -> "🛑 Stopped VM for " <> name c
  VMDestroyed c -> "🗑️  Destroyed VM at " <> toS (stateDir c)
  VMFailed c r -> "❌ VM failed for " <> name c <> ": " <> r
  VMConnectingShell c -> "🐚 Connecting to shell for " <> name c
  ProcessSpawned c a -> "🔧 Spawned process: " <> c <> " " <> T.unwords a
  ProcessExited c e -> "📤 Process exited: " <> c <> " (code: " <> T.pack (show e) <> ")"
  ProcessStopped c -> "⏹️  Process stopped gracefully: " <> c
  ProcessSigKilled c sig -> "🔪 Process force killed: " <> c <> " (signal: " <> T.pack (show sig) <> ")"
  ProcessTimeout c timeoutVal -> "⏰ Process timed out: " <> c <> " (after: " <> T.pack (show timeoutVal) <> "μs)"
  ProcessWaitingForExit c -> "⏳ Waiting for process exit: " <> c
  ProcessIOWaiting c -> "📡 Waiting for IO threads: " <> c
  ProcessGracefulStop c timeoutVal -> "🛑 Attempting graceful stop: " <> c <> " (timeout: " <> T.pack (show timeoutVal) <> "μs)"
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
  MainInfo m -> "✅ " <> m
  MainError e -> "❌ " <> e

-- | Set color based on LogLevel
setLogLevelColor :: (MonadIO m) => LogLevel -> m ()
setLogLevelColor level =
  liftIO $ setSGR $ case level of
    Trace -> [SetColor Foreground Dull Cyan]
    Debug -> [SetColor Foreground Dull Blue]
    Info -> [SetColor Foreground Vivid Green]
    Error -> [SetColor Foreground Vivid Red]
    Critical -> [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

-- | Create a logger that outputs to stderr with colors
vmLogger :: IOTracer AgentVmTrace
vmLogger = IOTracer $ Tracer $ \traceEvent -> do
  let level = traceLevel traceEvent
      traceMessage = renderTrace traceEvent
  setLogLevelColor level
  hPutStr stderr $ "[" <> logLevelText level <> "] "
  liftIO $ setSGR [Reset]
  hPutStrLn stderr traceMessage

-- | Create a level-aware logger that outputs to stderr with colors
vmLevelLogger :: IOTracer (LogLevel, AgentVmTrace)
vmLevelLogger = IOTracer $ Tracer $ \(level, traceEvent) -> do
  setLogLevelColor level
  hPutStr stderr $ "[" <> logLevelText level <> "] "
  liftIO $ setSGR [Reset]
  hPutStrLn stderr $ renderTrace traceEvent

-- | Create a log context for integration tests
createLogContext :: IO (IOTracer AgentVmTrace)
createLogContext = pure vmLogger
