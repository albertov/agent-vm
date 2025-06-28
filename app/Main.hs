{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

-- | Main entry point for agent-vm CLI
module Main (main) where

import AgentVM (loadVMConfig)
import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Git (generateDefaultName)
import AgentVM.Log
  ( AgentVmTrace (..),
    LogLevel (..),
    MonadTrace,
    renderTracedMessage,
    trace,
    traceLevel,
  )
import AgentVM.Monad (runVMT)
import AgentVM.Types (VMConfig (..), VMError (..), VMState (..), vmConfigFile2)
import qualified AgentVM.Types as Types
import AgentVM.VMStatus (renderVMStatus)
import Control.Concurrent.Thread.Delay (delay)
import Data.Generics.Labels ()
import Data.Generics.Product (HasType, typed)
import Data.Text (split)
import qualified Data.Text as T
import Lens.Micro.Mtl (view)
import Options.Applicative
import Plow.Logging (IOTracer (..), Tracer (..), filterTracer, traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude hiding (throwIO, trace)
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import UnliftIO (MonadUnliftIO, hFlush, throwIO)
import UnliftIO.Directory (makeAbsolute)

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser opts
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let mkEnv vmConfig = AgentVmEnv {tracer, vmConfig}
    case cmd of
      Create createConfig -> flip runVMT handleCreate . mkEnv =<< createConfigToVMConfig globalOpts createConfig
      Update updateConfig -> flip runVMT handleUpdate . mkEnv =<< updateConfigToVMConfig globalOpts updateConfig
      Start maybeVmName -> flip runVMT handleStart . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      Stop maybeVmName -> flip runVMT handleStop . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      Status maybeVmName -> flip runVMT handleStatus . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      Shell maybeVmName -> flip runVMT handleShell . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      Reset maybeVmName -> flip runVMT handleReset . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      Destroy maybeVmName -> flip runVMT handleDestroy . mkEnv =<< loadVMConfig' tracer globalOpts maybeVmName
      _ -> do
        traceWith tracer $ MainError ("Haskell agent-vm: " <> show cmd <> " (not yet implemented)")
        exitFailure
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "VM control command for managing development VMs"
            <> header "agent-vm - Agent sandbox VM lifecycle management"
        )
    loadVMConfig' tracer globalOpts maybeVmName = do
      vmName <- getVmName maybeVmName
      vmConfigFile2 (optStateDir globalOpts) vmName >>= loadVMConfig >>= \case
        Just vmConfig -> pure vmConfig
        Nothing -> do
          let msg = "Could not find config for " <> vmName
          traceWith tracer $ MainError msg
          throwIO $ ConfigError msg

data GlobalOpts = GlobalOpts
  { optStateDir :: Maybe FilePath,
    optVerbose :: Bool,
    optDebug :: Bool
  }

data CreateConfig = CreateConfig
  { createWorkspace :: Maybe FilePath,
    createName :: Maybe Text,
    createTmpfs :: Maybe Bool,
    createMemory :: Maybe Int,
    createCores :: Maybe Int,
    createDiskSize :: Maybe Int,
    createAdditionalPaths :: Maybe [Text],
    createPort :: Maybe Int,
    createSystemPackages :: Maybe [Text],
    createUid :: Maybe Int,
    createGid :: Maybe Int,
    createGroup :: Maybe Text,
    createShellName :: Maybe Text,
    createFlake :: Maybe Text,
    createNixBaseConfig :: Maybe FilePath
  }
  deriving (Show)

data Command
  = Create CreateConfig
  | Update CreateConfig
  | List
  | Reset (Maybe Text)
  | Start (Maybe Text)
  | Stop (Maybe Text)
  | Status (Maybe Text)
  | Destroy (Maybe Text)
  | Shell (Maybe Text)
  deriving (Show)

-- | Parse command line arguments
parseArgs :: Parser (GlobalOpts, Command)
parseArgs = (,) <$> globalOpts <*> commandParser
  where
    globalOpts =
      GlobalOpts
        <$> optional
          ( strOption
              ( long "state-dir"
                  <> metavar "DIR"
                  <> help "Override default state directory"
              )
          )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Enable verbose logging"
          )
        <*> switch
          ( long "debug"
              <> short 'd'
              <> help "Enable debug logging"
          )

    commandParser =
      hsubparser
        ( command "create" (info (Create <$> parseCreateConfig) (progDesc "Create a new VM with workspace directory"))
            <> command "update" (info (Update <$> parseCreateConfig) (progDesc "Update VM configuration without removing qcow2 image"))
            <> command "start" (info (Start <$> optional nameOption) (progDesc "Start VM"))
            <> command "stop" (info (Stop <$> optional nameOption) (progDesc "Stop VM"))
            <> command "status" (info (Status <$> optional nameOption) (progDesc "Show VM status"))
            <> command "list" (info (pure List) (progDesc "List all VMs"))
            <> command "reset" (info (Reset <$> optional nameOption) (progDesc "Delete the VM hard disk which holds all persistent state (except workspace)"))
            <> command "destroy" (info (Destroy <$> optional nameOption) (progDesc "Destroy VM"))
            <> command "shell" (info (Shell <$> optional nameOption) (progDesc "Connect to VM shell via serial console"))
        )

-- | Parse comma-separated list
parseCommaSeparated :: Mod OptionFields Text -> Parser [Text]
parseCommaSeparated opts = fmap (split (== ',')) (strOption opts)

nameOption :: Parser Text
nameOption =
  strOption
    ( long "name"
        <> metavar "NAME"
        <> help "VM name to connect to shell (optional)"
        <> short 'n'
    )

-- | Parse create command configuration
parseCreateConfig :: Parser CreateConfig
parseCreateConfig =
  CreateConfig
    <$> optional
      ( argument
          str
          ( metavar "WORKSPACE"
              <> help "Workspace directory path (default: current directory)"
          )
      )
    <*> optional nameOption
    <*> optional
      ( switch
          ( long "ephemeral"
              <> help "Use tmpfs for root filesystem (default: false)"
              <> short 'e'
          )
      )
    <*> optional
      ( option
          auto
          ( long "memory-size"
              <> metavar "GB"
              <> help "Memory in GB"
              <> short 'm'
          )
      )
    <*> optional
      ( option
          auto
          ( long "cores"
              <> metavar "CORES"
              <> help "Number of CPU cores"
              <> short 'c'
          )
      )
    <*> optional
      ( option
          auto
          ( long "disk-size"
              <> metavar "GB"
              <> help "Disk size in GB"
              <> short 'S'
          )
      )
    <*> optional
      ( parseCommaSeparated
          ( long "additional-paths"
              <> metavar "PATHS"
              <> help "Additional Nix paths (comma-separated)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "port"
              <> metavar "PORT"
              <> help "Port number"
              <> short 'p'
          )
      )
    <*> optional
      ( parseCommaSeparated
          ( long "system-packages"
              <> metavar "PACKAGES"
              <> help "System packages (comma-separated)"
          )
      )
    <*> optional
      ( option
          auto
          ( long "uid"
              <> metavar "UID"
              <> help "User ID"
              <> short 'u'
          )
      )
    <*> optional
      ( option
          auto
          ( long "gid"
              <> metavar "GID"
              <> help "Group ID"
              <> short 'g'
          )
      )
    <*> optional
      ( strOption
          ( long "group"
              <> metavar "GROUP"
              <> help "Group name"
              <> short 'G'
          )
      )
    <*> optional
      ( strOption
          ( long "shell"
              <> metavar "SHELL"
              <> help "Shell name"
              <> short 's'
          )
      )
    <*> optional
      ( strOption
          ( long "flake"
              <> metavar "FLAKE"
              <> help "Flake path"
              <> short 'F'
          )
      )
    <*> optional
      ( strOption
          ( long "base"
              <> metavar "PATH"
              <> help "Nix base configuration file path"
              <> short 'b'
          )
      )

-- | Convert CreateConfig to VMConfig for update, using existing config as defaults
updateConfigToVMConfig :: GlobalOpts -> CreateConfig -> IO VMConfig
updateConfigToVMConfig globalOpts updateConfig = do
  workspaceDir <- makeAbsolute (fromMaybe "." (createWorkspace updateConfig))
  vmName <- case createName updateConfig of
    Just vmName -> pure vmName
    Nothing -> generateDefaultName workspaceDir

  vmConfigFile' <- vmConfigFile2 (optStateDir globalOpts) vmName
  existingConfig <- loadVMConfig vmConfigFile'

  case existingConfig of
    Nothing -> do
      -- No existing config, update should fail
      throwIO $ VMNotFound vmConfigFile'
    Just existing -> do
      nixBaseConfig <- mapM makeAbsolute (createNixBaseConfig updateConfig <|> nixBaseConfig existing)
      flake <- sanitizeFlake (fromMaybe (flake existing) (createFlake updateConfig))
      -- Merge with existing config, CLI args override existing values
      pure
        existing
          { workspace = toS workspaceDir, -- Always update workspace to the provided value
            tmpfs = fromMaybe (tmpfs existing) (createTmpfs updateConfig),
            memorySize = fromMaybe (memorySize existing) (createMemory updateConfig),
            cores = fromMaybe (cores existing) (createCores updateConfig),
            diskSize = fromMaybe (diskSize existing) (createDiskSize updateConfig),
            additionalPaths = fromMaybe (additionalPaths existing) (createAdditionalPaths updateConfig),
            port = fromMaybe (port existing) (createPort updateConfig),
            systemPackages = fromMaybe (systemPackages existing) (createSystemPackages updateConfig),
            uid = fromMaybe (uid existing) (createUid updateConfig),
            gid = fromMaybe (gid existing) (createGid updateConfig),
            Types.group = fromMaybe (Types.group existing) (createGroup updateConfig),
            shellName = fromMaybe (shellName existing) (createShellName updateConfig),
            flake,
            nixBaseConfig,
            name = fromMaybe (name existing) (createName updateConfig)
          }

-- | Convert CreateConfig to VMConfig using defVMConfig for defaults
createConfigToVMConfig :: GlobalOpts -> CreateConfig -> IO VMConfig
createConfigToVMConfig globalOpts createConfig = do
  workspaceDir <- makeAbsolute (fromMaybe "." (createWorkspace createConfig))
  vmName <- case createName createConfig of
    Just vmName -> pure vmName
    Nothing -> generateDefaultName workspaceDir

  defaults <- Types.defVMConfig (optStateDir globalOpts) vmName (toS workspaceDir)
  nixBaseConfig <- mapM makeAbsolute (createNixBaseConfig createConfig <|> nixBaseConfig defaults)
  flake <- sanitizeFlake (fromMaybe (flake defaults) (createFlake createConfig))
  pure
    defaults
      { tmpfs = fromMaybe (tmpfs defaults) (createTmpfs createConfig),
        memorySize = fromMaybe (memorySize defaults) (createMemory createConfig),
        cores = fromMaybe (cores defaults) (createCores createConfig),
        diskSize = fromMaybe (diskSize defaults) (createDiskSize createConfig),
        additionalPaths = fromMaybe (additionalPaths defaults) (createAdditionalPaths createConfig),
        port = fromMaybe (port defaults) (createPort createConfig),
        systemPackages = fromMaybe (systemPackages defaults) (createSystemPackages createConfig),
        uid = fromMaybe (uid defaults) (createUid createConfig),
        gid = fromMaybe (gid defaults) (createGid createConfig),
        Types.group = fromMaybe (Types.group defaults) (createGroup createConfig),
        shellName = fromMaybe (shellName defaults) (createShellName createConfig),
        flake,
        nixBaseConfig
      }

type MonadVmCli m r =
  ( MonadVM m,
    MonadUnliftIO m,
    MonadReader r m,
    HasType VMConfig r,
    MonadTrace AgentVmTrace m
  )

-- | Handle the create command
handleCreate :: (MonadVmCli m r) => m ()
handleCreate = do
  -- Run the VM create operation
  result <- create =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to create VM: " <> show err)
      exitFailure'
    Right () -> do
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("Successfully created VM: " <> name')
      exitSuccess'

-- | Handle the update command
handleUpdate :: (MonadVmCli m r) => m ()
handleUpdate = do
  -- Run the VM update operation
  result <- update =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to update VM: " <> show err)
      exitFailure'
    Right () -> do
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("Successfully updated VM: " <> name')
      exitSuccess'

-- | Handle the start command
handleStart :: (MonadVmCli m r) => m ()
handleStart = do
  result <- start =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to start VM: " <> show err)
      exitFailure'
    Right () -> do
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("Successfully started VM: " <> name')
      exitSuccess'

-- | Handle the stop command
handleStop :: (MonadVmCli m r) => m ()
handleStop = do
  result <- stop =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to stop VM: " <> show err)
      exitFailure'
    Right () -> do
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("Successfully stopped VM: " <> name')
      exitSuccess'

-- | Handle the status command
handleStatus :: (MonadVmCli m r) => m ()
handleStatus = do
  result <- status =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to get VM status: " <> toS (show err :: [Char]))
      exitFailure'
    Right vmState -> do
      -- Display basic VM state
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("VM " <> name' <> " is " <> showVMState vmState)

      -- Display detailed status information when running
      case vmState of
        Running vmStatus -> do
          let statusLines = renderVMStatus vmStatus
          mapM_ (trace . MainInfo) statusLines
        _ -> pure ()
      exitSuccess'

-- | Handle the shell command
handleShell :: (MonadVmCli m r) => m ()
handleShell = do
  -- Run the VM shell connection operation
  result <- shell =<< view typed
  case result of
    Left err -> do
      trace $ MainError ("Failed to connect to shell: " <> show err)
      exitFailure'
    Right () -> do
      name' <- view (typed @VMConfig . #name)
      trace $ MainInfo ("Shell session ended for VM: " <> name')
      exitSuccess'

-- | Handle the reset command with confirmation
handleReset :: (MonadVmCli m r) => m ()
handleReset = do
  name' <- view (typed @VMConfig . #name)
  confirmed <- liftIO $ askConfirmation ("Are you sure you want to reset VM '" <> name' <> "'? This will delete the disk image but keep the configuration. [y/N]")
  if confirmed
    then do
      result <- reset =<< view typed
      case result of
        Left err -> do
          trace $ MainError ("Failed to reset VM: " <> show err)
          exitFailure'
        Right () -> do
          trace $ MainInfo ("Successfully reset VM: " <> name')
          exitSuccess'
    else do
      trace $ MainInfo "Reset cancelled"
      exitSuccess'

-- | Handle the destroy command with confirmation
handleDestroy :: (MonadVmCli m r) => m ()
handleDestroy = do
  name' <- view (typed @VMConfig . #name)
  confirmed <- liftIO $ askConfirmation ("Are you sure you want to destroy VM '" <> name' <> "'? This will permanently delete all VM data. [y/N]")
  if confirmed
    then do
      result <- destroy =<< view typed
      case result of
        Left err -> do
          trace $ MainError ("Failed to destroy VM: " <> show err)
          exitFailure'
        Right () -> do
          trace $ MainInfo ("Successfully destroyed VM: " <> name')
          exitSuccess'
    else do
      trace $ MainInfo "Destroy cancelled"
      exitSuccess'

getVmName :: Maybe Text -> IO Text
getVmName = maybe (generateDefaultName =<< getCurrentDirectory) pure

-- | Ask user for confirmation with a yes/no prompt
askConfirmation :: Text -> IO Bool
askConfirmation prompt = do
  putStr ((toS prompt :: [Char]) <> " ")
  hFlush stdout
  response <- getLine
  pure $ case T.toLower (T.strip (toS response)) of
    "y" -> True
    "yes" -> True
    _ -> False

-- | Show VMState as text
showVMState :: VMState -> Text
showVMState Stopped = "stopped"
showVMState Starting = "starting"
showVMState (Running _) = "running"
showVMState Stopping = "stopping"
showVMState Failed = "failed"

-- | Set up async filtered logger based on global options
-- Verbose flag sets minimum level to Debug, Debug flag sets it to Trace
withAsyncFilteredLogger :: GlobalOpts -> (IOTracer AgentVmTrace -> IO a) -> IO a
withAsyncFilteredLogger globalOpts tracerAction = do
  let minLevel = case (optDebug globalOpts, optVerbose globalOpts) of
        (True, _) -> Trace -- Debug flag takes precedence
        (False, True) -> Debug -- Verbose flag
        (False, False) -> Info -- Default level
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    let filteredTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
        filteredTracer = filterTracer (\traceEvent -> traceLevel traceEvent >= minLevel) $
          Tracer $ \traceEvent -> do
            let IOTracer (Tracer textTracerFunc) = asyncTracer
            textTracerFunc (renderTracedMessage traceEvent)
    tracerAction (IOTracer filteredTracer)

sanitizeFlake :: Text -> IO Text
sanitizeFlake flake
  | dir : _ <- T.splitOn "#" flake = makeAbsoluteIfDirectory dir
  | otherwise = makeAbsoluteIfDirectory flake
  where
    makeAbsoluteIfDirectory :: Text -> IO Text
    makeAbsoluteIfDirectory (toS -> dir) =
      doesDirectoryExist dir >>= \case
        True -> toS <$> makeAbsolute dir
        False -> pure flake

exitSuccess' :: (MonadIO m) => m ()
exitSuccess' = liftIO $ do
  delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
  exitSuccess

exitFailure' :: (MonadIO m) => m ()
exitFailure' = liftIO $ do
  delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
  exitFailure
