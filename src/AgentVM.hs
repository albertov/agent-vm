{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NumericUnderscores #-}
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
import AgentVM.VMStatus (getDetailedVMStatus)
import Control.Concurrent.Thread.Delay (delay)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Lens.Micro
import Protolude hiding (bracket, throwIO, trace, try, tryJust)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catchAny, throwIO, try, tryJust)

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
  stop config =
    try $ do
      trace (Log.VMStopping config)
      stopVM config
  status config = do
    try $ do
      vmState <- getVMState config
      trace (Log.VMStatusChecked config)
      return vmState

  shell config = do
    try $ do
      trace (Log.VMConnectingShell config)
      connectToVMShell config

  reset config =
    try $ do
      resetVM config
        <* trace (Log.VMReset config)

-- | Destroy a VM and clean up its resources
destroyVM ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m ()
destroyVM config = do
  -- First stop the VM if it's running
  vmState <- getVMState config
  case vmState of
    Running _ -> stopVM config
    _ -> pure ()

  -- Remove the entire state directory
  let stateDir' = stateDir config
  stateDirExists <- liftIO $ doesDirectoryExist stateDir'
  when stateDirExists $ liftIO $ removeDirectoryRecursive stateDir'

-- | Reset a VM by deleting its disk image but keeping configuration
resetVM ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m ()
resetVM config = do
  -- First stop the VM if it's running
  vmState <- getVMState config
  case vmState of
    Running _ -> stopVM config
    _ -> pure ()

  -- Remove only the disk image
  let diskPath = vmDiskImage config
  diskExists <- liftIO $ doesFileExist diskPath
  when diskExists $ liftIO $ removeFile diskPath

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

-- | Get the current state of a VM by checking its PID file
getVMState ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m VMState
getVMState config = do
  let pidFilePath = vmPidFile config
  -- Read and parse PID from file
  ePidContent <-
    tryJust
      (guard . isDoesNotExistError)
      (liftIO (readFile pidFilePath))
  case ePidContent of
    Right pidContent -> do
      let pidText :: Text = T.strip (toS pidContent)
      case readMaybe (T.unpack pidText) of
        Nothing -> do
          trace $ Log.VMFailed config ("Invalid PID in file: " <> toS pidFilePath)
          return Failed
        Just (pid :: ProcessID) -> do
          -- Check if process is still running by trying to send signal 0
          isRunning <-
            liftIO $
              catchAny
                (signalProcess 0 pid >> return True)
                (\_ -> return False)
          if isRunning
            then do
              -- Get detailed status when running
              vmStatus <- liftIO $ getDetailedVMStatus config (fromIntegral pid)
              return (Running vmStatus)
            else return Stopped
    Left _ -> return Stopped

-- | Stop a VM by reading its PID file and sending termination signals
stopVM ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m ()
stopVM config = do
  let pidFilePath = vmPidFile config
  -- Read and parse PID from file
  ePidContent <-
    tryJust
      (guard . isDoesNotExistError)
      (liftIO (readFile pidFilePath))
  case ePidContent of
    Right pidContent -> do
      let pidText :: Text = T.strip (toS pidContent)
      case readMaybe (T.unpack pidText) of
        Nothing -> do
          trace $ Log.VMFailed config ("Invalid PID in file: " <> toS pidFilePath)
          liftIO $ throwIO $ WorkspaceError ("Invalid PID format in " <> toS pidFilePath)
        Just (pid :: ProcessID) -> do
          -- Try to terminate the process gracefully with SIGTERM
          liftIO $
            catchAny
              ( do
                  signalProcess sigTERM pid
                  -- Wait a moment for graceful shutdown
                  delay 5_000_000 -- 5 seconds
                  -- Check if process is still running by sending signal 0
                  catchAny
                    (signalProcess sigKILL pid >> removeFile pidFilePath)
                    (\_ -> removeFile pidFilePath) -- Process already dead, just clean up
              )
              ( \_ -> do
                  -- Process might already be dead, try to clean up PID file anyway
                  catchAny (removeFile pidFilePath) (\_ -> pure ())
              )

          trace $ Log.VMStopped config
    Left _ -> do
      trace $ Log.VMFailed config ("VM PID file not found, VM may already be stopped: " <> toS pidFilePath)
      liftIO $ throwIO $ VMNotFound pidFilePath

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
