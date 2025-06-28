{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Main entry point for agent-vm CLI
module Main (main) where

import AgentVM (loadVMConfig)
import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv (..))
import qualified AgentVM.Env as Env
import AgentVM.Log (AgentVmTrace (..), LogLevel (..), renderTracedMessage, traceLevel)
import AgentVM.Monad (runVMT)
import AgentVM.Types (VMConfig (..), VMError (..), vmConfigFile)
import qualified AgentVM.Types as Types
import Control.Concurrent.Thread.Delay (delay)
import Data.Text (split)
import qualified Data.Text as T
import Options.Applicative
import Plow.Logging (IOTracer (..), Tracer (..), filterTracer, traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude hiding (throwIO)
import qualified Shelly as Sh
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeBaseName)
import UnliftIO (catchAny, throwIO)
import UnliftIO.Directory (makeAbsolute)

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
            <> command "reset" (info (Reset <$> optional nameOption) (progDesc "Delete the VM hard disk clearing all persistent state (except workspace)"))
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

-- | Get current git branch name from workspace directory
getGitBranch :: FilePath -> IO (Maybe Text)
getGitBranch workspaceDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workspaceDir, "branch", "--show-current"]))
      (\_ -> pure Nothing)
  pure $ fmap (T.strip . toS) result

-- | Get git repository name from remote origin in workspace directory
getGitRepoName :: FilePath -> IO (Maybe Text)
getGitRepoName workspaceDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workspaceDir, "remote", "show", "origin"]))
      (\_ -> pure Nothing)
  case result of
    Nothing -> pure Nothing
    Just output -> do
      let lines' = T.lines (toS output)
          fetchUrl = find (T.isPrefixOf "  Fetch URL: ") lines'
      pure $ do
        url <- fetchUrl
        let urlPart = T.drop 13 url -- Remove "  Fetch URL: "
        -- Extract repo name from various URL formats
            repoName = case T.splitOn "/" urlPart of
              [] -> Nothing
              parts -> case reverse parts of
                [] -> Nothing
                lastPart : _ ->
                  let -- Remove .git suffix if present
                      cleaned =
                        if T.isSuffixOf ".git" lastPart
                          then T.dropEnd 4 lastPart
                          else lastPart
                   in if T.null cleaned then Nothing else Just cleaned
        repoName

-- | Get repository name fallback using workspace directory basename
getRepoNameFallback :: FilePath -> Text
getRepoNameFallback workspaceDir = toS $ takeBaseName workspaceDir

-- | Sanitize name by replacing dashes with underscores
sanitizeName :: Text -> Text
sanitizeName = T.replace "-" "_"

-- | Generate default VM name from git repository and branch in workspace
generateDefaultName :: FilePath -> IO Text
generateDefaultName workspaceDir = do
  maybeBranch <- getGitBranch workspaceDir
  maybeRepo <- getGitRepoName workspaceDir

  let branch = fromMaybe "main" maybeBranch
      repo = fromMaybe (getRepoNameFallback workspaceDir) maybeRepo

  pure $ sanitizeName repo <> "-" <> sanitizeName branch

-- | Convert CreateConfig to VMConfig for update, using existing config as defaults
updateConfigToVMConfig :: GlobalOpts -> CreateConfig -> IO VMConfig
updateConfigToVMConfig globalOpts updateConfig = do
  workspaceDir <- makeAbsolute (fromMaybe "." (createWorkspace updateConfig))
  vmName <- case createName updateConfig of
    Just vmName -> pure vmName
    Nothing -> generateDefaultName workspaceDir

  -- First create a temporary config to determine the correct paths
  tempConfig <- Types.defVMConfig (optStateDir globalOpts) vmName (toS workspaceDir)

  -- Now we can get the correct config file path
  let configFilePath = vmConfigFile tempConfig

  existingConfig <- loadVMConfig configFilePath

  case existingConfig of
    Nothing -> do
      -- No existing config, update should fail
      throwIO $ WorkspaceError ("VM does not exist. Use 'create' to create a new VM: " <> toS configFilePath)
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

-- | Handle the create command
handleCreate :: GlobalOpts -> CreateConfig -> IO ()
handleCreate globalOpts createConfig = do
  vmConfig <- createConfigToVMConfig globalOpts createConfig

  -- Set up async filtered logging
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let env = AgentVmEnv {tracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM create operation
    result <- runVMT env (create vmConfig)
    case result of
      Left err -> do
        traceWith tracer $ MainError ("Failed to create VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith tracer $ MainInfo ("Successfully created VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the update command
handleUpdate :: GlobalOpts -> CreateConfig -> IO ()
handleUpdate globalOpts updateConfig = do
  vmConfig <- updateConfigToVMConfig globalOpts updateConfig

  -- Set up async filtered logging
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let env = AgentVmEnv {tracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM update operation
    result <- runVMT env (update vmConfig)
    case result of
      Left err -> do
        traceWith tracer $ MainError ("Failed to update VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith tracer $ MainInfo ("Successfully updated VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the start command
handleStart :: GlobalOpts -> Maybe Text -> IO ()
handleStart globalOpts maybeVmName = do
  case maybeVmName of
    Nothing -> do
      -- No VM name provided, try to infer from current directory
      currentDir <- getCurrentDirectory
      vmName <- generateDefaultName currentDir
      startVmWithName globalOpts vmName
    Just vmName -> startVmWithName globalOpts vmName

-- | Start VM with given name
startVmWithName :: GlobalOpts -> Text -> IO ()
startVmWithName globalOpts vmName = do
  vmConfig <- Types.defVMConfig (optStateDir globalOpts) vmName "."

  -- Set up async filtered logging
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let env = AgentVmEnv {tracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM start operation
    result <- runVMT env (start vmConfig)
    case result of
      Left err -> do
        traceWith tracer $ MainError ("Failed to start VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith tracer $ MainInfo ("Successfully started VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the stop command
handleStop :: GlobalOpts -> Maybe Text -> IO ()
handleStop globalOpts maybeVmName = do
  case maybeVmName of
    Nothing -> do
      -- No VM name provided, try to infer from current directory
      currentDir <- getCurrentDirectory
      vmName <- generateDefaultName currentDir
      stopVmWithName globalOpts vmName
    Just vmName -> stopVmWithName globalOpts vmName

-- | Stop VM with given name
stopVmWithName :: GlobalOpts -> Text -> IO ()
stopVmWithName globalOpts vmName = do
  vmConfig <- Types.defVMConfig (optStateDir globalOpts) vmName "."

  -- Set up async filtered logging
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let env = AgentVmEnv {tracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM stop operation
    result <- runVMT env (stop vmConfig)
    case result of
      Left err -> do
        traceWith tracer $ MainError ("Failed to stop VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith tracer $ MainInfo ("Successfully stopped VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the shell command
handleShell :: GlobalOpts -> Maybe Text -> IO ()
handleShell globalOpts maybeVmName = do
  case maybeVmName of
    Nothing -> do
      -- No VM name provided, try to infer from current directory
      currentDir <- getCurrentDirectory
      vmName <- generateDefaultName currentDir
      connectWithShell globalOpts vmName
    Just vmName -> connectWithShell globalOpts vmName

-- | Connect to VM shell using the library function
connectWithShell :: GlobalOpts -> Text -> IO ()
connectWithShell globalOpts vmName = do
  vmConfig <- Types.defVMConfig (optStateDir globalOpts) vmName "."

  -- Set up async filtered logging
  withAsyncFilteredLogger globalOpts $ \tracer -> do
    let env = AgentVmEnv {tracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM shell connection operation
    result <- runVMT env (shell vmConfig)
    case result of
      Left err -> do
        traceWith tracer $ MainError ("Failed to connect to shell: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith tracer $ MainInfo ("Shell session ended for VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser opts
  case cmd of
    Create createConfig -> handleCreate globalOpts createConfig
    Update updateConfig -> handleUpdate globalOpts updateConfig
    Start maybeVmName -> handleStart globalOpts maybeVmName
    Stop maybeVmName -> handleStop globalOpts maybeVmName
    Shell maybeVmName -> handleShell globalOpts maybeVmName
    _ -> do
      -- Set up async filtered logging for unimplemented commands
      withAsyncFilteredLogger globalOpts $ \tracer -> do
        -- For now, just demonstrate logging is working for other commands
        traceWith tracer $ MainError ("Haskell agent-vm: " <> toS (show cmd :: [Char]) <> " (not yet implemented)")
        exitFailure
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "VM control command for managing development VMs"
            <> header "agent-vm - Type-safe VM lifecycle management"
        )
