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
    traceLevel,
    vmLogger,
    vmLevelLogger,
    createLogContext,
    (<&),
  )
where

import AgentVM.Types (BranchName, VMConfig, unBranchName)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer), traceWith)
import Protolude
  ( Eq,
    FilePath,
    Generic,
    IO,
    Int,
    Integer,
    Monad,
    MonadIO,
    Ord,
    Show,
    Text,
    pure,
    show,
    ($),
    (.),
    (<>),
  )
import System.Console.ANSI (Color (Blue, Cyan, Green, Red), ColorIntensity (Dull, Vivid), ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground), SGR (Reset, SetColor, SetConsoleIntensity))
import qualified System.Console.ANSI as ANSI
import System.IO (Handle, stderr)
import UnliftIO (liftIO)

(<&) :: Tracer m a -> a -> m ()
(<&) = traceWith

class (Monad m) => MonadTrace trace m | m -> trace where
  trace :: trace -> m ()

-- | MonadIO version of setSGR
setSGR :: (MonadIO m) => [SGR] -> m ()
setSGR = liftIO . ANSI.setSGR

-- | MonadIO version of Text.IO.hPutStr
hPutStr :: (MonadIO m) => Handle -> Text -> m ()
hPutStr h = liftIO . TIO.hPutStr h

-- | MonadIO version of Text.IO.hPutStrLn
hPutStrLn :: (MonadIO m) => Handle -> Text -> m ()
hPutStrLn h = liftIO . TIO.hPutStrLn h

-- | Log level wrapper for filtering and importance tagging
data LogLevel a
  = Critical a
  | Error a
  | Info a
  | Debug a
  | Trace a
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Extract the wrapped value from LogLevel
unLogLevel :: LogLevel a -> a
unLogLevel = \case
  Critical a -> a
  Error a -> a
  Info a -> a
  Debug a -> a
  Trace a -> a

-- | Get the level as text
logLevelText :: LogLevel a -> Text
logLevelText = \case
  Critical _ -> "CRITICAL"
  Error _ -> "ERROR"
  Info _ -> "INFO"
  Debug _ -> "DEBUG"
  Trace _ -> "TRACE"

-- | Render a LogLevel with a custom renderer for the wrapped value
renderLogLevel :: (a -> Text) -> LogLevel a -> Text
renderLogLevel render level =
  "[" <> logLevelText level <> "] " <> render (unLogLevel level)

-- | Tag an AgentVmTrace with its appropriate LogLevel
traceLevel :: AgentVmTrace -> LogLevel AgentVmTrace
traceLevel traceEvent = case traceEvent of
  -- Critical events - system failures that need immediate attention
  VMFailed {} -> Critical traceEvent
  AgentServiceFailed {} -> Critical traceEvent
  -- Error events - failures that should be investigated
  ProcessError {} -> Error traceEvent
  ProcessTimeout {} -> Error traceEvent
  ProcessSigKilled {} -> Error traceEvent
  SSHFailed {} -> Error traceEvent
  NixBuildFailed {} -> Error traceEvent
  -- Info events - normal operational events
  VMCreated {} -> Info traceEvent
  VMStarted {} -> Info traceEvent
  VMStopped {} -> Info traceEvent
  VMDestroyed {} -> Info traceEvent
  SSHConnected {} -> Info traceEvent
  NixBuildCompleted {} -> Info traceEvent
  PortAllocated {} -> Info traceEvent
  WorkspaceCreated {} -> Info traceEvent
  AgentServiceHealthy {} -> Info traceEvent
  -- Debug events - detailed operational info
  VMStarting {} -> Debug traceEvent
  VMStopping {} -> Debug traceEvent
  ProcessSpawned {} -> Debug traceEvent
  ProcessExited {} -> Debug traceEvent
  ProcessStopped {} -> Debug traceEvent
  ProcessWaitingForExit {} -> Debug traceEvent
  ProcessIOWaiting {} -> Debug traceEvent
  ProcessGracefulStop {} -> Debug traceEvent
  SSHKeyGenerated {} -> Debug traceEvent
  SSHConnecting {} -> Debug traceEvent
  SSHCommandExecuted {} -> Debug traceEvent
  NixBuildStarted {} -> Debug traceEvent
  PortScanning {} -> Debug traceEvent
  PortReleased {} -> Debug traceEvent
  WorkspaceCloned {} -> Debug traceEvent
  WorkspaceSynced {} -> Debug traceEvent
  WorkspaceRemoved {} -> Debug traceEvent
  AgentServiceStarting {} -> Debug traceEvent
  -- Trace events - very detailed output
  ProcessOutput {} -> Trace traceEvent
  NixBuildProgress {} -> Trace traceEvent

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
  | ProcessStopped Text -- Process gracefully stopped
  | ProcessSigKilled Text Int -- Process force killed with signal
  | ProcessTimeout Text Integer -- Process timed out after N microseconds
  | ProcessWaitingForExit Text -- Waiting for process to exit
  | ProcessIOWaiting Text -- Waiting for IO threads to complete
  | ProcessGracefulStop Text Integer -- Attempting graceful stop with timeout
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert trace to message (full implementation)
traceToMessage :: AgentVmTrace -> Text
traceToMessage = renderTrace

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
  ProcessStopped c -> "⏹️  Process stopped gracefully: " <> c
  ProcessSigKilled c sig -> "🔪 Process force killed: " <> c <> " (signal: " <> T.pack (show sig) <> ")"
  ProcessTimeout c timeout -> "⏰ Process timed out: " <> c <> " (after: " <> T.pack (show timeout) <> "μs)"
  ProcessWaitingForExit c -> "⏳ Waiting for process exit: " <> c
  ProcessIOWaiting c -> "📡 Waiting for IO threads: " <> c
  ProcessGracefulStop c timeout -> "🛑 Attempting graceful stop: " <> c <> " (timeout: " <> T.pack (show timeout) <> "μs)"
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

-- | Set color based on LogLevel
setLogLevelColor :: (MonadIO m) => LogLevel a -> m ()
setLogLevelColor level =
  setSGR $ case level of
    Trace _ -> [SetColor Foreground Dull Cyan]
    Debug _ -> [SetColor Foreground Dull Blue]
    Info _ -> [SetColor Foreground Vivid Green]
    Error _ -> [SetColor Foreground Vivid Red]
    Critical _ -> [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]

-- | Create a logger that outputs to stderr with colors
vmLogger :: IOTracer AgentVmTrace
vmLogger = IOTracer $ Tracer $ \traceEvent -> do
  let leveledTrace = traceLevel traceEvent
      traceMessage = renderTrace traceEvent
  setLogLevelColor leveledTrace
  hPutStr stderr $ "[" <> logLevelText leveledTrace <> "] "
  setSGR [Reset]
  hPutStrLn stderr traceMessage

-- | Create a level-aware logger that outputs to stderr with colors
vmLevelLogger :: IOTracer (LogLevel AgentVmTrace)
vmLevelLogger = IOTracer $ Tracer $ \leveledTrace -> do
  setLogLevelColor leveledTrace
  hPutStr stderr $ "[" <> logLevelText leveledTrace <> "] "
  setSGR [Reset]
  hPutStrLn stderr $ renderTrace (unLogLevel leveledTrace)

-- | Create a log context for integration tests
createLogContext :: IO (IOTracer AgentVmTrace)
createLogContext = pure vmLogger
