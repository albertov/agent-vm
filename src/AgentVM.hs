{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
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
import AgentVM.VMCache (addVMToCache, removeVMFromCache)
import AgentVM.VMStatus (getDetailedVMStatus)
import Control.Concurrent.Thread.Delay (delay)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Lens.Micro
import Protolude hiding (bracket, handleJust, throwIO, trace, try, tryJust)
import System.Directory (doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.FilePath (takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.Posix.Signals (sigKILL, sigTERM, signalProcess)
import System.Posix.Types (ProcessID)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (catchAny, handleJust, throwIO, try, tryJust)

-- | Extract global state directory from VM state directory
-- e.g., "/home/user/.local/share/agent-vm/my-vm" -> Just "/home/user/.local/share/agent-vm"
getGlobalStateDir :: VMConfig -> Maybe FilePath
getGlobalStateDir config =
  let vmStatePath = config ^. #stateDir
   in Just $ takeDirectory vmStatePath

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
      -- Add VM to cache after successful creation
      let workspacePath = config ^. #workspace
          vmName = config ^. #name
          globalStateDir = getGlobalStateDir config
      addVMToCache globalStateDir workspacePath vmName
      trace (Log.VMCreated config)

  update config =
    try $ do
      buildVMImage config
      -- Update VM in cache in case workspace changed
      let workspacePath = config ^. #workspace
          vmName = config ^. #name
          globalStateDir = getGlobalStateDir config
      addVMToCache globalStateDir workspacePath vmName
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

  -- Remove VM from cache before removing files
  let workspacePath = config ^. #workspace
      globalStateDir = getGlobalStateDir config
  removeVMFromCache globalStateDir workspacePath

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
  interactWith stdin stdout stderr (shellEscapeKey config) $ Process scriptPath []

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
          isRunning <- isPidRunning pid
          if isRunning
            then do
              -- Get detailed status when running
              vmStatus <- liftIO $ getDetailedVMStatus config (fromIntegral pid)
              return (Running vmStatus)
            else return Stopped
    Left _ -> return Stopped

isPidRunning :: (MonadIO m) => ProcessID -> m Bool
isPidRunning pid =
  liftIO $
    catchAny
      (signalProcess 0 pid >> return True)
      (\_ -> return False)

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
          handleJust (guard . isDoesNotExistError) (const (pure ())) $ do
            liftIO $ do
              signalProcess sigTERM pid
              let loop :: Int -> IO ()
                  loop 0 = signalProcess sigKILL pid
                  loop n = do
                    isPidRunning pid >>= \case
                      True -> delay 1_000 >> loop (n - 1)
                      False -> pure ()
              loop 50
          liftIO $ removeFile pidFilePath
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
  interactWith stdin stdout stderr (shellEscapeKey config) $ Socket serialSocketPath
