{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Main entry point for agent-vm CLI
module Main (main) where

import AgentVM (loadVMConfig)
import AgentVM.Class (MonadVM (..))
import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Log (AgentVmTrace (..), LogLevel (..), renderTracedMessage, traceLevel)
import AgentVM.Monad (runVMT)
import AgentVM.Types (VMConfig (..), VMError (..), VMState (..), vmConfigFile2)
import qualified AgentVM.Types as Types
import Control.Concurrent.Thread.Delay (delay)
import Data.Generics.Labels ()
import Data.Text (split)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime)
import Lens.Micro ((^.))
import Options.Applicative
import Plow.Logging (IOTracer (..), Tracer (..), filterTracer, traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude hiding (throwIO)
import qualified Shelly as Sh
import System.Directory (doesDirectoryExist, getCurrentDirectory)
import System.FilePath (takeBaseName)
import Text.Printf (printf)
import UnliftIO (catchAny, throwIO)
import UnliftIO.Directory (makeAbsolute)

-- | Detailed VM status information
data VMStatus = VMStatus
  { vmStatusPid :: Maybe Int,
    vmStatusMemoryInfo :: Maybe MemoryInfo,
    vmStatusCPUInfo :: Maybe CPUInfo,
    vmStatusGitInfo :: Maybe GitInfo
  }
  deriving (Show, Eq)

-- | Memory information from /proc/[pid]/status
data MemoryInfo = MemoryInfo
  { memVmPeak :: Maybe Int64, -- in bytes
    memVmSize :: Maybe Int64, -- in bytes
    memVmRSS :: Maybe Int64 -- in bytes
  }
  deriving (Show, Eq)

-- | CPU information from /proc/[pid]/stat
data CPUInfo = CPUInfo
  { cpuUserTime :: NominalDiffTime,
    cpuSystemTime :: NominalDiffTime
  }
  deriving (Show, Eq)

-- | Git repository information
newtype GitInfo = GitInfo
  { gitLastCommitDiff :: Text
  }
  deriving (Show, Eq)

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
    let env = AgentVmEnv {tracer, vmConfig}

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
    let env = AgentVmEnv {tracer, vmConfig}

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
startVmWithName globalOpts vmName =
  withAgentVmEnv globalOpts vmName $ \env -> do
    -- Run the VM start operation
    result <- runVMT env (start (env ^. #vmConfig))
    case result of
      Left err -> do
        traceWith (tracer env) $ MainError ("Failed to start VM: " <> show err)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith (tracer env) $ MainInfo ("Successfully started VM: " <> vmName)
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
stopVmWithName globalOpts vmName =
  withAgentVmEnv globalOpts vmName $ \env -> do
    -- Run the VM stop operation
    result <- runVMT env (stop (env ^. #vmConfig))
    case result of
      Left err -> do
        traceWith (tracer env) $ MainError ("Failed to stop VM: " <> show err)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith (tracer env) $ MainInfo ("Successfully stopped VM: " <> vmName)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitSuccess

-- | Handle the status command
handleStatus :: GlobalOpts -> Maybe Text -> IO ()
handleStatus globalOpts maybeVmName = do
  case maybeVmName of
    Nothing -> do
      -- No VM name provided, try to infer from current directory
      currentDir <- getCurrentDirectory
      vmName <- generateDefaultName currentDir
      statusVmWithName globalOpts vmName
    Just vmName -> statusVmWithName globalOpts vmName

-- | Get detailed status for VM with given name
statusVmWithName :: GlobalOpts -> Text -> IO ()
statusVmWithName globalOpts vmName =
  withAgentVmEnv globalOpts vmName $ \env -> do
    -- Run the VM status operation
    result <- runVMT env (status (env ^. #vmConfig))
    case result of
      Left err -> do
        traceWith (tracer env) $ MainError ("Failed to get VM status: " <> toS (show err :: [Char]))
        delay 1_000_000
        exitFailure
      Right vmState -> do
        -- Display basic VM state
        traceWith (tracer env) $ MainInfo ("VM " <> vmName <> " is " <> showVMState vmState)

        -- Get detailed status information
        when (isRunning vmState) $ do
          vmStatus <- liftIO $ getDetailedVMStatus (env ^. #vmConfig)
          let statusLines = renderVMStatus vmStatus
          mapM_ (traceWith (tracer env) . MainInfo) statusLines

        delay 1_000_000
        exitSuccess

-- | Show VMState as text
showVMState :: VMState -> Text
showVMState Stopped = "stopped"
showVMState Starting = "starting"
showVMState Running = "running"
showVMState Stopping = "stopping"
showVMState Failed = "failed"

-- | Check if VMState represents a running state
isRunning :: VMState -> Bool
isRunning Running = True
isRunning _ = False

-- | Render VMStatus to list of text lines
renderVMStatus :: VMStatus -> [Text]
renderVMStatus vmStatus =
  let pidInfo = case vmStatusPid vmStatus of
        Nothing -> ["No PID file found"]
        Just pid -> ["Process ID: " <> toS (show pid :: [Char])]

      memInfo = maybe [] renderMemoryInfo (vmStatusMemoryInfo vmStatus)

      cpuInfo = maybe [] renderCPUInfo (vmStatusCPUInfo vmStatus)

      gitInfo = maybe ["No git repository or no commits"] renderGitInfo (vmStatusGitInfo vmStatus)
   in pidInfo ++ memInfo ++ cpuInfo ++ gitInfo

-- | Render memory information
renderMemoryInfo :: MemoryInfo -> [Text]
renderMemoryInfo memInfo =
  catMaybes
    [ fmap (formatMemoryBytes "VmPeak:") (memVmPeak memInfo),
      fmap (formatMemoryBytes "VmSize:") (memVmSize memInfo),
      fmap (formatMemoryBytes "VmRSS:") (memVmRSS memInfo)
    ]

-- | Format memory bytes to MB/GB with 2 decimals
formatMemoryBytes :: Text -> Int64 -> Text
formatMemoryBytes fieldName bytes =
  let mbValue = fromIntegral bytes / (1024 * 1024) :: Double
      gbValue = mbValue / 1024
      formatted =
        if gbValue >= 1.0
          then T.pack (printf "%.2f GB" gbValue)
          else T.pack (printf "%.2f MB" mbValue)
   in fieldName <> " " <> formatted

-- | Render CPU information
renderCPUInfo :: CPUInfo -> [Text]
renderCPUInfo cpuInfo =
  [ "CPU User Time: " <> formatCPUTime (cpuUserTime cpuInfo),
    "CPU System Time: " <> formatCPUTime (cpuSystemTime cpuInfo)
  ]

-- | Format NominalDiffTime as human-readable text
formatCPUTime :: NominalDiffTime -> Text
formatCPUTime time =
  let seconds = realToFrac time :: Double
   in T.pack (printf "%.2f seconds" seconds)

-- | Render git information
renderGitInfo :: GitInfo -> [Text]
renderGitInfo gitInfo =
  [ "Last commit diff:",
    gitLastCommitDiff gitInfo
  ]

-- | Get detailed VM status including memory, CPU usage, and git info
getDetailedVMStatus :: VMConfig -> IO VMStatus
getDetailedVMStatus config = do
  let pidFilePath = Types.vmPidFile config

  -- Try to read PID file and get process info
  ePidContent <- catchAny (Just <$> readFile pidFilePath) (\_ -> pure Nothing)

  case ePidContent of
    Nothing -> pure $ VMStatus Nothing Nothing Nothing Nothing
    Just pidContent -> do
      let pidText = T.strip (toS pidContent)
      case readMaybe (T.unpack pidText) of
        Nothing -> pure $ VMStatus Nothing Nothing Nothing Nothing
        Just pid -> do
          memInfo <- getMemoryInfo pid
          cpuInfo <- getCPUInfo pid
          gitInfo <- getGitInfo (workspace config)
          pure $ VMStatus (Just pid) memInfo cpuInfo gitInfo

-- | Get memory information from /proc filesystem
getMemoryInfo :: Int -> IO (Maybe MemoryInfo)
getMemoryInfo pid = do
  let statusPath = "/proc/" <> show pid <> "/status"

  statusInfo <- catchAny (readFileLines statusPath) (\_ -> pure [])

  let extractAndParseField prefix = do
        line <- find (T.isPrefixOf prefix) statusInfo
        parseMemoryLine line

  if null statusInfo
    then pure Nothing
    else
      pure $
        Just $
          MemoryInfo
            (extractAndParseField "VmPeak:")
            (extractAndParseField "VmSize:")
            (extractAndParseField "VmRSS:")

-- | Parse memory line from /proc/[pid]/status and convert kB to bytes
parseMemoryLine :: Text -> Maybe Int64
parseMemoryLine line =
  case T.words line of
    (_ : valueStr : "kB" : _) ->
      case readMaybe (T.unpack valueStr) of
        Just (kbValue :: Int64) -> Just (kbValue * 1024) -- Convert kB to bytes
        Nothing -> Nothing
    _ -> Nothing

-- | Get CPU information from /proc filesystem
getCPUInfo :: Int -> IO (Maybe CPUInfo)
getCPUInfo pid = do
  let statPath = "/proc/" <> show pid <> "/stat"

  statInfo <- catchAny (readFile statPath) (\_ -> pure "")

  case T.words (toS statInfo) of
    fields
      | length fields > 14 -> do
          let utimeStr = case drop 13 fields of
                x : _ -> x
                [] -> "0"
              stimeStr = case drop 14 fields of
                x : _ -> x
                [] -> "0"

          case (readMaybe (T.unpack utimeStr), readMaybe (T.unpack stimeStr)) of
            (Just (utime :: Int64), Just (stime :: Int64)) -> do
              -- Convert clock ticks to seconds (typical USER_HZ is 100)
              let clockTicksPerSec = 100 :: Double
                  utimeSeconds = fromIntegral utime / clockTicksPerSec
                  stimeSeconds = fromIntegral stime / clockTicksPerSec
              pure $
                Just $
                  CPUInfo
                    (realToFrac utimeSeconds)
                    (realToFrac stimeSeconds)
            _ -> pure Nothing
    _ -> pure Nothing

-- | Read file and split into lines
readFileLines :: FilePath -> IO [Text]
readFileLines path = T.lines . toS <$> readFile path

-- | Get git information from workspace
getGitInfo :: FilePath -> IO (Maybe GitInfo)
getGitInfo workspaceDir = do
  lastCommitDiff <- getLastCommitDiff workspaceDir
  case lastCommitDiff of
    Nothing -> pure Nothing
    Just diffText -> pure $ Just $ GitInfo diffText

-- | Get last commit diff using git log -p
getLastCommitDiff :: FilePath -> IO (Maybe Text)
getLastCommitDiff workspaceDir = do
  catchAny
    ( Sh.shelly $ Sh.silently $ do
        commitHash <- Sh.run "git" ["-C", toS workspaceDir, "rev-parse", "HEAD"]
        diffOutput <- Sh.run "git" ["-C", toS workspaceDir, "log", "-p", T.strip (toS commitHash), "-1"]
        pure $ Just (T.strip (toS diffOutput))
    )
    (\_ -> pure Nothing)

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
connectWithShell globalOpts vmName =
  withAgentVmEnv globalOpts vmName $ \env -> do
    -- Run the VM shell connection operation
    result <- runVMT env (shell (env ^. #vmConfig))
    case result of
      Left err -> do
        traceWith (tracer env) $ MainError ("Failed to connect to shell: " <> show err)
        delay 1_000_000 -- work around bug in withAsyncHandleTracer which doesn't wait for messages to finisih printing
        exitFailure
      Right () -> do
        traceWith (tracer env) $ MainInfo ("Shell session ended for VM: " <> vmName)
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
    Status maybeVmName -> handleStatus globalOpts maybeVmName
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

withAgentVmEnv :: GlobalOpts -> Text -> (AgentVmEnv -> IO a) -> IO a
withAgentVmEnv globalOpts vmName fun =
  withAsyncFilteredLogger globalOpts $ \tracer ->
    vmConfigFile2 (optStateDir globalOpts) vmName >>= loadVMConfig >>= \case
      Just vmConfig ->
        fun AgentVmEnv {tracer, vmConfig}
      Nothing -> do
        let msg = "Could not find config for " <> vmName
        traceWith tracer $ MainError msg
        throwIO $ ConfigError msg
