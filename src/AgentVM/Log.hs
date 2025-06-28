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
  -- Info events - normal operational events
  VMCreated {} -> Info
  VMUpdated {} -> Info
  VMDestroyed {} -> Info
  VMConnectingShell {} -> Info
  WorkspaceCreated {} -> Info
  -- Debug events - detailed operational info
  VMStarting {} -> Debug
  ProcessSpawned {} -> Debug
  NixBuildStarted {} -> Debug
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
  | VMDestroyed VMConfig
  | VMFailed VMConfig Text
  | VMConnectingShell VMConfig
  | -- Process Management
    ProcessSpawned Text [Text]
  | -- Nix Operations
    NixBuildStarted Text
  | -- Workspace Operations
    WorkspaceCreated FilePath
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
  VMDestroyed c -> "🗑️  Destroyed VM at " <> toS (stateDir c)
  VMFailed c r -> "❌ VM failed for " <> name c <> ": " <> r
  VMConnectingShell c -> "🐚 Connecting to shell for " <> name c
  ProcessSpawned c a -> "🔧 Spawned process: " <> c <> " " <> T.unwords a
  NixBuildStarted f -> "🔨 Building " <> f
  WorkspaceCreated p -> "📁 Created workspace: " <> T.pack p
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
