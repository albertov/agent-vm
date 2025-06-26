{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main entry point for agent-vm CLI
module Main (main) where

import AgentVM (loadVMConfig)
import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv (..))
import qualified AgentVM.Env as Env
import AgentVM.Log (AgentVmTrace (..), renderTracedMessage)
import AgentVM.Monad (runVMT)
import AgentVM.Types (VMConfig (..), VMError (..), vmConfigFile)
import qualified AgentVM.Types as Types
import Control.Concurrent.Thread.Delay (delay)
import Data.Functor.Contravariant (contramap)
import Data.Text (split)
import qualified Data.Text as T
import Options.Applicative
import Plow.Logging (traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude hiding (throwIO)
import qualified Shelly as Sh
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import UnliftIO (catchAny, throwIO)
import UnliftIO.Directory (makeAbsolute)

data GlobalOpts = GlobalOpts
  { optStateDir :: Maybe FilePath,
    _optVerbose :: Bool,
    _optDebug :: Bool,
    _optTimeout :: Int
  }

data CreateConfig = CreateConfig
  { createName :: Maybe Text,
    createWorkspace :: FilePath,
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
  | Start (Maybe Text)
  | Stop
  | Status
  | Logs
  | List
  | Destroy
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
        <*> option
          auto
          ( long "timeout"
              <> short 't'
              <> metavar "SECONDS"
              <> value 120
              <> help "Global timeout for VM operations"
          )

    commandParser =
      hsubparser
        ( command "create" (info (Create <$> parseCreateConfig) (progDesc "Create a new VM with workspace directory"))
            <> command "update" (info (Update <$> parseCreateConfig) (progDesc "Update VM configuration without removing qcow2 image"))
            <> command "start" (info (Start <$> parseStartConfig) (progDesc "Start VM"))
            <> command "stop" (info (pure Stop) (progDesc "Stop VM"))
            <> command "status" (info (pure Status) (progDesc "Show VM status"))
            <> command "logs" (info (pure Logs) (progDesc "Show VM logs"))
            <> command "list" (info (pure List) (progDesc "List all VMs"))
            <> command "destroy" (info (pure Destroy) (progDesc "Destroy VM"))
            <> command "shell" (info (Shell <$> parseShellConfig) (progDesc "Connect to VM shell via serial console"))
        )

-- | Parse comma-separated list
parseCommaSeparated :: Mod OptionFields Text -> Parser [Text]
parseCommaSeparated opts = fmap (split (== ',')) (strOption opts)

-- | Parse start command configuration
parseStartConfig :: Parser (Maybe Text)
parseStartConfig =
  optional
    ( strOption
        ( long "name"
            <> metavar "NAME"
            <> help "VM name to start (optional)"
        )
    )

-- | Parse shell command configuration
parseShellConfig :: Parser (Maybe Text)
parseShellConfig =
  optional
    ( strOption
        ( long "name"
            <> metavar "NAME"
            <> help "VM name to connect to shell (optional)"
        )
    )

-- | Parse create command configuration
parseCreateConfig :: Parser CreateConfig
parseCreateConfig =
  CreateConfig
    <$> optional
      ( strOption
          ( long "name"
              <> metavar "NAME"
              <> help "VM name (optional, defaults to repo_name-branch_name)"
          )
      )
    <*> argument
      str
      ( metavar "WORKSPACE"
          <> help "Workspace directory path"
      )
    <*> optional
      ( switch
          ( long "tmpfs"
              <> help "Use tmpfs for root filesystem"
          )
      )
    <*> optional
      ( option
          auto
          ( long "memory"
              <> metavar "GB"
              <> help "Memory in GB"
          )
      )
    <*> optional
      ( option
          auto
          ( long "cores"
              <> metavar "CORES"
              <> help "Number of CPU cores"
          )
      )
    <*> optional
      ( option
          auto
          ( long "disk-size"
              <> metavar "GB"
              <> help "Disk size in GB"
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
          )
      )
    <*> optional
      ( option
          auto
          ( long "gid"
              <> metavar "GID"
              <> help "Group ID"
          )
      )
    <*> optional
      ( strOption
          ( long "group"
              <> metavar "GROUP"
              <> help "Group name"
          )
      )
    <*> optional
      ( strOption
          ( long "shell-name"
              <> metavar "SHELL"
              <> help "Shell name"
          )
      )
    <*> optional
      ( strOption
          ( long "flake"
              <> metavar "FLAKE"
              <> help "Flake path"
          )
      )
    <*> optional
      ( strOption
          ( long "nix-base-config"
              <> metavar "PATH"
              <> help "Nix base configuration file path"
          )
      )

-- | Get current git branch name from workspace directory
getGitBranch :: FilePath -> IO (Maybe Text)
getGitBranch workspaceDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workspaceDir, "branch", "--show-current"]))
      (\_ -> return Nothing)
  return $ fmap (T.strip . toS) result

-- | Get git repository name from remote origin in workspace directory
getGitRepoName :: FilePath -> IO (Maybe Text)
getGitRepoName workspaceDir = do
  result <-
    catchAny
      (Just <$> Sh.shelly (Sh.run "git" ["-C", toS workspaceDir, "remote", "show", "origin"]))
      (\_ -> return Nothing)
  case result of
    Nothing -> return Nothing
    Just output -> do
      let lines' = T.lines (toS output)
          fetchUrl = find (T.isPrefixOf "  Fetch URL: ") lines'
      return $ do
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

  let branch = maybe "main" sanitizeName maybeBranch
      repo = maybe (sanitizeName (getRepoNameFallback workspaceDir)) sanitizeName maybeRepo

  return $ repo <> "-" <> branch

-- | Convert CreateConfig to VMConfig for update, using existing config as defaults
updateConfigToVMConfig :: GlobalOpts -> CreateConfig -> IO VMConfig
updateConfigToVMConfig globalOpts updateConfig = do
  workspaceDir <- makeAbsolute (createWorkspace updateConfig)
  vmName <- case createName updateConfig of
    Just vmName -> return vmName
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
            flake = fromMaybe (flake existing) (createFlake updateConfig),
            nixBaseConfig,
            name = fromMaybe (name existing) (createName updateConfig)
          }

-- | Convert CreateConfig to VMConfig using defVMConfig for defaults
createConfigToVMConfig :: GlobalOpts -> CreateConfig -> IO VMConfig
createConfigToVMConfig globalOpts createConfig = do
  workspaceDir <- makeAbsolute (createWorkspace createConfig)
  vmName <- case createName createConfig of
    Just vmName -> return vmName
    Nothing -> generateDefaultName workspaceDir

  defaults <- Types.defVMConfig (optStateDir globalOpts) vmName (toS workspaceDir)
  nixBaseConfig <- mapM makeAbsolute (createNixBaseConfig createConfig <|> nixBaseConfig defaults)
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
        flake = fromMaybe (flake defaults) (createFlake createConfig),
        nixBaseConfig
      }

-- | Handle the create command
handleCreate :: GlobalOpts -> CreateConfig -> IO ()
handleCreate globalOpts createConfig = do
  vmConfig <- createConfigToVMConfig globalOpts createConfig

  -- Set up async logging with a buffer of 1000 messages
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    -- Create the final tracer that converts AgentVmTrace to Text
    let finalTracer = contramap renderTracedMessage asyncTracer
        env = AgentVmEnv {tracer = finalTracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM create operation
    result <- runVMT env (create vmConfig)
    case result of
      Left err -> do
        traceWith finalTracer $ MainError ("Failed to create VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith finalTracer $ MainInfo ("Successfully created VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the update command
handleUpdate :: GlobalOpts -> CreateConfig -> IO ()
handleUpdate globalOpts updateConfig = do
  vmConfig <- updateConfigToVMConfig globalOpts updateConfig

  -- Set up async logging with a buffer of 1000 messages
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    -- Create the final tracer that converts AgentVmTrace to Text
    let finalTracer = contramap renderTracedMessage asyncTracer
        env = AgentVmEnv {tracer = finalTracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM update operation
    result <- runVMT env (update vmConfig)
    case result of
      Left err -> do
        traceWith finalTracer $ MainError ("Failed to update VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith finalTracer $ MainInfo ("Successfully updated VM: " <> name vmConfig)
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

  -- Set up async logging with a buffer of 1000 messages
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    -- Create the final tracer that converts AgentVmTrace to Text
    let finalTracer = contramap renderTracedMessage asyncTracer
        env = AgentVmEnv {tracer = finalTracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM start operation
    result <- runVMT env (start vmConfig)
    case result of
      Left err -> do
        traceWith finalTracer $ MainError ("Failed to start VM: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith finalTracer $ MainInfo ("Successfully started VM: " <> name vmConfig)
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

  -- Set up async logging with a buffer of 1000 messages
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    -- Create the final tracer that converts AgentVmTrace to Text
    let finalTracer = contramap renderTracedMessage asyncTracer
        env = AgentVmEnv {tracer = finalTracer, Env.stateDir = Types.stateDir vmConfig}

    -- Run the VM shell connection operation
    result <- runVMT env (shell vmConfig)
    case result of
      Left err -> do
        traceWith finalTracer $ MainError ("Failed to connect to shell: " <> toS (show err :: [Char]))
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith finalTracer $ MainInfo ("Shell session ended for VM: " <> name vmConfig)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser opts
  case cmd of
    Create createConfig -> handleCreate globalOpts createConfig
    Update updateConfig -> handleUpdate globalOpts updateConfig
    Start maybeVmName -> handleStart globalOpts maybeVmName
    Shell maybeVmName -> handleShell globalOpts maybeVmName
    _ -> do
      -- Set up async logging with a buffer of 1000 messages
      withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
        -- Create the final tracer that converts AgentVmTrace to Text
        let finalTracer = contramap renderTracedMessage asyncTracer

        -- For now, just demonstrate logging is working for other commands
        traceWith finalTracer $ MainError ("Haskell agent-vm: " <> toS (show cmd :: [Char]) <> " (not yet implemented)")
        exitFailure
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "VM control command for managing development VMs"
            <> header "agent-vm - Type-safe VM lifecycle management"
        )
