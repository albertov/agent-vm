{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Main entry point for the agent-vm library
module AgentVM
  ( -- * Types
    VMConfig (..),
    VMError (..),

    -- * Logging
    AgentVmTrace,
    traceToMessage,
    renderTrace,
    vmLogger,

    -- * Interface Monad
    MonadVM (..),

    -- * Config
    loadVMConfig,

    -- * Environment
    AgentVmEnv (..),
    runVMT,
  )
where

import AgentVM.Class (MonadVM (..))
import AgentVM.Env
import AgentVM.Interactive
import AgentVM.Log (AgentVmTrace, MonadTrace (..), renderTrace, traceToMessage, vmLogger)
import qualified AgentVM.Log as Log
import AgentVM.Monad (VMT, runVMT)
import AgentVM.Nix
import AgentVM.StreamingProcess (Process (..))
import AgentVM.StreamingSocket (Socket (..))
import AgentVM.Types
import Data.Generics.Labels ()
import Lens.Micro
import Protolude hiding (bracket, throwIO, trace, try)
import System.Directory (doesDirectoryExist, doesFileExist)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (throwIO, try)

-- | Instance of MonadVM for VMT IO
instance (MonadUnliftIO m) => MonadVM (VMT m) where
  create config =
    try $ do
      let vmDirPath = config ^. #stateDir
      dirExists <- liftIO $ doesDirectoryExist vmDirPath
      when dirExists $
        throwIO $
          WorkspaceError ("Directory already exists: " <> toS vmDirPath)
      buildVMImage config
      trace (Log.VMCreated config)

  update config =
    try $ do
      buildVMImage config
      trace (Log.VMUpdated config)

  destroy config =
    try $ do
      destroyVM config
        <* trace (Log.VMDestroyed config)

  -- These are not implemented yet
  start config = do
    try $ do
      trace (Log.VMStarting config)
      startVM config
  stop _ = return (Left (VMInvalidState "stop not implemented"))
  status _ = return (Left (VMInvalidState "status not implemented"))

  shell config = do
    try $ do
      trace (Log.VMConnectingShell config)
      connectToVMShell config

-- | Destroy a VM and clean up its resources
destroyVM ::
  ( MonadTrace AgentVmTrace m
  ) =>
  VMConfig ->
  m ()
destroyVM = const (pure ()) -- TODO

-- | Start a VM and relay stdin/stdout using raw streaming process with proper terminal handling
startVM ::
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  VMConfig ->
  m ()
startVM config = do
  let scriptPath = vmStartScript config

  -- Check if VM start script exists
  scriptExists <- liftIO $ doesFileExist scriptPath
  unless scriptExists $ do
    trace $ Log.VMFailed config ("VM start script not found: " <> toS scriptPath)
    liftIO $ throwIO $ VMNotFound scriptPath

  trace $ Log.ProcessSpawned (toS scriptPath) []
  interactWith stdin stdout stderr $ Process scriptPath []

-- | Connect to VM shell via serial socket with proper terminal handling
connectToVMShell ::
  ( MonadTrace AgentVmTrace m,
    MonadIO m
  ) =>
  VMConfig ->
  m ()
connectToVMShell config = do
  let serialSocketPath = vmSerialSocket config

  -- Check if serial socket exists
  socketExists <- liftIO $ doesFileExist serialSocketPath
  unless socketExists $ do
    trace $ Log.VMFailed config ("Serial socket not found: " <> toS serialSocketPath)
    liftIO $ throwIO $ VMNotFound serialSocketPath

  trace $ Log.ProcessSpawned "socket-connection" [toS serialSocketPath]
  interactWith stdin stdout stderr $ Socket serialSocketPath
