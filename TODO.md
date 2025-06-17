# Haskell agent-vm Implementation TODO

## Project Setup

### Initialize Haskell Project Structure
```bash
mkdir -p agent-vm-hs/{src,test,app}
cd agent-vm-hs
```

### Create cabal file
```cabal
cabal-version:      3.0
name:               agent-vm
version:            0.1.0.0
synopsis:           Type-safe VM lifecycle management with Nix integration
description:        QEMU VM management system with compile-time state guarantees
license:            MIT
author:             Agent VM Team
maintainer:         dev@agent-vm.org
category:           System
build-type:         Simple

common warnings
    ghc-options: -Wall -Wcompat -Widentities -Wincomplete-record-updates
                 -Wincomplete-uni-patterns -Wmissing-export-lists
                 -Wmissing-home-modules -Wpartial-fields -Wredundant-constraints
                 -threaded -rtsopts -with-rtsopts=-N

library
    import:           warnings
    exposed-modules:  AgentVM
                    , AgentVM.Types
                    , AgentVM.State
                    , AgentVM.Process
                    , AgentVM.SSH
                    , AgentVM.Nix
                    , AgentVM.Log
                    , AgentVM.Config
    other-modules:    AgentVM.Internal
    build-depends:    base ^>=4.17.0.0
                    , typed-process
                    , async
                    , stm
                    , containers
                    , text
                    , bytestring
                    , aeson
                    , yaml
                    , plow-log
                    , microlens
                    , generic-lens
                    , optparse-applicative
                    , ansi-terminal
                    , directory
                    , filepath
                    , time
                    , network
                    , libssh2
                    , mtl
                    , transformers
                    , exceptions
    hs-source-dirs:   src
    default-language: Haskell2010

executable agent-vm
    import:           warnings
    main-is:          Main.hs
    build-depends:    base ^>=4.17.0.0
                    , agent-vm
                    , optparse-applicative
    hs-source-dirs:   app
    default-language: Haskell2010

test-suite agent-vm-test
    import:           warnings
    type:             exitcode-stdio-1.0
    main-is:          Spec.hs
    other-modules:    AgentVM.StateSpec
                    , AgentVM.ProcessSpec
                    , AgentVM.IntegrationSpec
    build-depends:    base ^>=4.17.0.0
                    , agent-vm
                    , hspec
                    , hspec-discover
                    , QuickCheck
                    , temporary
    hs-source-dirs:   test
    default-language: Haskell2010
```

## Core Type Definitions

### VM State Machine Types (`src/AgentVM/Types.hs`)
```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module AgentVM.Types where

import Data.Text (Text)
import Data.Time (UTCTime)
import System.Process.Typed (Process)

-- | VM states as type-level values
data VMState = Stopped | Starting | Running | Stopping | Failed

-- | Branch name newtype for type safety
newtype BranchName = BranchName { unBranchName :: Text }
  deriving (Eq, Ord, Show)

-- | VM identifier
data VMId = VMId
  { vmIdBranch :: BranchName
  , vmIdHost :: Text
  } deriving (Eq, Ord, Show)

-- | VM configuration
data VMConfig = VMConfig
  { vmConfigHost :: Text
  , vmConfigPort :: Int
  , vmConfigSshPort :: Int
  , vmConfigMemory :: Int
  , vmConfigCores :: Int
  , vmConfigWorkspace :: FilePath
  , vmConfigNixPath :: FilePath
  } deriving (Eq, Show)

-- | Type-safe VM with phantom state
data VM (s :: VMState) = VM
  { vmId :: VMId
  , vmConfig :: VMConfig
  , vmCreatedAt :: UTCTime
  , vmStateData :: VMStateData s
  }

-- | State-specific data
data VMStateData (s :: VMState) where
  StoppedData :: VMStateData 'Stopped
  StartingData :: { startingPid :: Int } -> VMStateData 'Starting
  RunningData :: { runningPid :: Int, runningProcess :: Process () () () } -> VMStateData 'Running
  StoppingData :: { stoppingPid :: Int } -> VMStateData 'Stopping
  FailedData :: { failureReason :: Text } -> VMStateData 'Failed

deriving instance Show (VMStateData s)

-- | VM operations GADT for type-safe transitions
data VMOp :: VMState -> VMState -> * -> * where
  Create :: VMConfig -> VMOp 'Stopped 'Stopped VMId
  Start :: VMOp 'Stopped 'Running (Process () () ())
  Stop :: VMOp 'Running 'Stopped ()
  ForceStop :: VMOp 'Running 'Stopped ()
  MarkFailed :: Text -> VMOp s 'Failed ()

-- | Errors that can occur during VM operations
data VMError
  = VMAlreadyExists BranchName
  | VMNotFound BranchName
  | VMInvalidState Text
  | VMStartupTimeout
  | VMShutdownTimeout
  | SSHConnectionFailed Text
  | NixBuildFailed Text
  | PortAllocationFailed
  | WorkspaceError Text
  deriving (Eq, Show)
```

### Logging Types (`src/AgentVM/Logging.hs`)
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AgentVM.Logging where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import PlowLog (LogAction, Message, Severity(..))
import qualified PlowLog

-- | All possible log events in the system
data AgentVmTrace
  = -- VM Lifecycle
    VMCreated { branch :: BranchName, config :: VMConfig }
  | VMStarting { branch :: BranchName }
  | VMStarted { branch :: BranchName, pid :: Int }
  | VMStopping { branch :: BranchName }
  | VMStopped { branch :: BranchName }
  | VMDestroyed { branch :: BranchName }
  | VMFailed { branch :: BranchName, reason :: Text }

  -- Process Management
  | ProcessSpawned { cmd :: Text, args :: [Text] }
  | ProcessExited { cmd :: Text, exitCode :: Int }
  | ProcessOutput { cmd :: Text, output :: Text }
  | ProcessError { cmd :: Text, error :: Text }

  -- SSH Operations
  | SSHKeyGenerated { keyPath :: FilePath }
  | SSHConnecting { host :: Text, port :: Int }
  | SSHConnected { host :: Text, port :: Int }
  | SSHCommandExecuted { cmd :: Text }
  | SSHFailed { reason :: Text }

  -- Nix Operations
  | NixBuildStarted { flakeRef :: Text }
  | NixBuildProgress { message :: Text }
  | NixBuildCompleted { storePath :: FilePath }
  | NixBuildFailed { error :: Text }

  -- Network Operations
  | PortScanning { startPort :: Int }
  | PortAllocated { port :: Int }
  | PortReleased { port :: Int }

  -- Workspace Operations
  | WorkspaceCreated { path :: FilePath }
  | WorkspaceCloned { origin :: Text, destination :: FilePath }
  | WorkspaceSynced { path :: FilePath }
  | WorkspaceRemoved { path :: FilePath }

  -- Agent Service
  | AgentServiceStarting { branch :: BranchName }
  | AgentServiceHealthy { branch :: BranchName, uptime :: Text }
  | AgentServiceFailed { branch :: BranchName, error :: Text }
  deriving (Show, Eq, Generic)

-- | Convert trace to colored text based on severity
traceToMessage :: AgentVmTrace -> Message
traceToMessage trace = PlowLog.Message
  { PlowLog.messageSeverity = traceSeverity trace
  , PlowLog.messageText = renderTrace trace
  }

-- | Determine severity from trace type
traceSeverity :: AgentVmTrace -> Severity
traceSeverity = \case
  VMFailed {} -> Error
  ProcessError {} -> Error
  SSHFailed {} -> Error
  NixBuildFailed {} -> Error
  AgentServiceFailed {} -> Error

  VMCreated {} -> Info
  VMStarted {} -> Info
  VMStopped {} -> Info
  SSHConnected {} -> Info
  NixBuildCompleted {} -> Info

  _ -> Debug

-- | Render trace as formatted text
renderTrace :: AgentVmTrace -> Text
renderTrace = \case
  VMCreated b _ -> "🆕 Created VM for branch " <> unBranchName b
  VMStarted b p -> "🚀 Started VM for " <> unBranchName b <> " (PID: " <> T.pack (show p) <> ")"
  VMStopped b -> "🛑 Stopped VM for " <> unBranchName b
  SSHConnected h p -> "🔗 SSH connected to " <> h <> ":" <> T.pack (show p)
  SSHFailed r -> "❌ SSH failed: " <> r
  NixBuildStarted f -> "🔧 Building " <> f
  NixBuildCompleted p -> "✅ Built " <> T.pack p
  PortAllocated p -> "🔌 Allocated port " <> T.pack (show p)
  AgentServiceHealthy b u -> "💚 Agent healthy on " <> unBranchName b <> " (up " <> u <> ")"
  -- ... etc for all constructors

-- | Create a logger that outputs to stdout with colors
vmLogger :: LogAction IO AgentVmTrace
vmLogger = PlowLog.cmap traceToMessage PlowLog.richMessageAction
```

## STM-based VM Registry

### Concurrent VM State Management (`src/AgentVM/State.hs`)
```haskell
{-# LANGUAGE RankNTypes #-}

module AgentVM.State where

import Control.Concurrent.STM
import Control.Monad (when)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

-- | Thread-safe VM registry
data VMRegistry = VMRegistry
  { vmMap :: TVar (Map BranchName VMInfo)
  , vmLocks :: TVar (Set BranchName)
  , vmPorts :: TVar (Set Int)
  }

-- | Runtime VM information
data VMInfo = forall s. VMInfo (VM s)

-- | Create a new VM registry
newVMRegistry :: IO VMRegistry
newVMRegistry = atomically $ VMRegistry
  <$> newTVar Map.empty
  <*> newTVar Set.empty
  <*> newTVar Set.empty

-- | Acquire exclusive lock on a branch
withVMLock :: VMRegistry -> BranchName -> IO a -> IO a
withVMLock registry branch action = do
  atomically $ do
    locks <- readTVar (vmLocks registry)
    when (branch `Set.member` locks) retry
    writeTVar (vmLocks registry) (Set.insert branch locks)

  finally action $ atomically $
    modifyTVar' (vmLocks registry) (Set.delete branch)

-- | Register a new VM
registerVM :: VMRegistry -> VM s -> STM ()
registerVM registry vm = do
  vms <- readTVar (vmMap registry)
  case Map.lookup (vmIdBranch $ vmId vm) vms of
    Just _ -> throwSTM $ VMAlreadyExists (vmIdBranch $ vmId vm)
    Nothing -> writeTVar (vmMap registry)
                (Map.insert (vmIdBranch $ vmId vm) (VMInfo vm) vms)

-- | Find a VM by branch name
lookupVM :: VMRegistry -> BranchName -> STM (Maybe VMInfo)
lookupVM registry branch = Map.lookup branch <$> readTVar (vmMap registry)

-- | Remove a VM from registry
unregisterVM :: VMRegistry -> BranchName -> STM ()
unregisterVM registry branch =
  modifyTVar' (vmMap registry) (Map.delete branch)

-- | Allocate a free port
allocatePort :: VMRegistry -> Int -> STM (Either VMError Int)
allocatePort registry startPort = do
  ports <- readTVar (vmPorts registry)
  let findFree p
        | p > startPort + 100 = Left PortAllocationFailed
        | p `Set.member` ports = findFree (p + 1)
        | otherwise = Right p
  case findFree startPort of
    Right port -> do
      writeTVar (vmPorts registry) (Set.insert port ports)
      return (Right port)
    Left err -> return (Left err)

-- | Release a port
releasePort :: VMRegistry -> Int -> STM ()
releasePort registry port =
  modifyTVar' (vmPorts registry) (Set.delete port)
```

## Process Management

### Nix Integration (`src/AgentVM/Nix.hs`)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module AgentVM.Nix where

import Control.Exception (bracket)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.Process.Typed
import AgentVM.Logging

-- | Build VM configuration using Nix
buildVMConfig :: LogAction IO AgentVmTrace -> BranchName -> FilePath -> IO (Either VMError FilePath)
buildVMConfig logger branch workspace = do
  logger <& NixBuildStarted flakeRef

  let proc = setStdin closed
           $ setStdout byteStringOutput
           $ setStderr byteStringOutput
           $ proc "nix" ["build", T.unpack flakeRef, "--no-link", "--print-out-paths"]

  withProcessWait proc $ \p -> do
    exitCode <- waitExitCode p
    stdout <- atomically $ getStdout p
    stderr <- atomically $ getStderr p

    case exitCode of
      ExitSuccess -> do
        let storePath = T.strip $ decodeUtf8 stdout
        logger <& NixBuildCompleted (T.unpack storePath)
        return $ Right (T.unpack storePath)
      ExitFailure _ -> do
        logger <& NixBuildFailed (decodeUtf8 stderr)
        return $ Left (NixBuildFailed $ decodeUtf8 stderr)
  where
    flakeRef = "path:" <> T.pack workspace <> "#vm-config." <> unBranchName branch

-- | Run Nix-generated VM script
runVMScript :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
runVMScript logger scriptPath = do
  let proc = setStdin closed
           $ setStdout createPipe
           $ setStderr createPipe
           $ proc scriptPath []

  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess proc
```

### SSH Management (`src/AgentVM/SSH.hs`)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module AgentVM.SSH where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Network.SSH.Client.LibSSH2
import System.FilePath ((</>))
import System.Process.Typed
import AgentVM.Types
import AgentVM.Logging

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey keyPath = do
  let proc = setStdin closed
           $ setStdout nullStream
           $ setStderr nullStream
           $ proc "ssh-keygen"
             [ "-t", "ed25519"
             , "-f", keyPath
             , "-N", ""
             , "-C", "agent-vm"
             ]

  runProcess_ proc

  -- Set proper permissions
  setFileMode keyPath 0o600
  setFileMode (keyPath <.> "pub") 0o644

-- | Wait for SSH to become available
waitForSSH :: LogAction IO AgentVmTrace -> Text -> Int -> FilePath -> Int -> IO Bool
waitForSSH logger host port keyPath maxAttempts = go maxAttempts
  where
    go 0 = do
      logger <& SSHFailed "Max attempts reached"
      return False
    go n = do
      logger <& SSHConnecting host port
      result <- trySSHConnect host port keyPath
      case result of
        Right () -> do
          logger <& SSHConnected host port
          return True
        Left _ -> do
          threadDelay 2_000_000  -- 2 seconds
          go (n - 1)

-- | Try to establish SSH connection
trySSHConnect :: Text -> Int -> FilePath -> IO (Either Text ())
trySSHConnect host port keyPath = do
  let proc = setStdin closed
           $ setStdout nullStream
           $ setStderr nullStream
           $ proc "ssh"
             [ "-o", "ConnectTimeout=5"
             , "-o", "StrictHostKeyChecking=no"
             , "-o", "UserKnownHostsFile=/dev/null"
             , "-i", keyPath
             , "-p", show port
             , "dev@" <> T.unpack host
             , "echo", "connected"
             ]

  exitCode <- runProcess proc
  case exitCode of
    ExitSuccess -> return $ Right ()
    ExitFailure _ -> return $ Left "Connection failed"

-- | Execute command over SSH
sshExec :: Text -> Int -> FilePath -> Text -> IO (Either Text Text)
sshExec host port keyPath command = do
  let proc = setStdin closed
           $ setStdout byteStringOutput
           $ setStderr byteStringOutput
           $ proc "ssh"
             [ "-o", "ConnectTimeout=10"
             , "-o", "StrictHostKeyChecking=no"
             , "-o", "UserKnownHostsFile=/dev/null"
             , "-i", keyPath
             , "-p", show port
             , "dev@" <> T.unpack host
             , T.unpack command
             ]

  (exitCode, stdout, stderr) <- readProcess proc
  case exitCode of
    ExitSuccess -> return $ Right (decodeUtf8 stdout)
    ExitFailure _ -> return $ Left (decodeUtf8 stderr)
```

## Configuration Parsing

### JSON Config Management (`src/AgentVM/Config.hs`)
```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AgentVM.Config where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics
import System.FilePath ((</>))

-- | VM configuration as stored in JSON
data VMConfigJson = VMConfigJson
  { vm_name :: Text
  , host :: Text
  , port :: Int
  , ssh_port :: Int
  , ssh_key_path :: FilePath
  , workspace_path :: FilePath
  , nix_config_path :: FilePath
  , created_at :: UTCTime
  , upstream_repo :: Maybe Text
  , host_uid :: Int
  , host_gid :: Int
  } deriving (Generic, Show)

instance FromJSON VMConfigJson where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON VMConfigJson where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Load VM configuration from state directory
loadVMConfig :: FilePath -> BranchName -> IO (Either String VMConfigJson)
loadVMConfig stateDir branch = do
  let configPath = stateDir </> T.unpack (unBranchName branch) </> "config.json"
  eitherDecodeFileStrict configPath

-- | Save VM configuration to state directory
saveVMConfig :: FilePath -> BranchName -> VMConfigJson -> IO ()
saveVMConfig stateDir branch config = do
  let configPath = stateDir </> T.unpack (unBranchName branch) </> "config.json"
  encodeFile configPath config
```

## CLI Implementation

### Main CLI Entry Point (`app/Main.hs`)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Options.Applicative
import AgentVM
import AgentVM.Types
import System.Exit (exitFailure)

data GlobalOpts = GlobalOpts
  { optStateDir :: Maybe FilePath
  , optVerbose :: Bool
  , optDebug :: Bool
  , optTimeout :: Int
  }

data Command
  = Create CreateOpts
  | Start (Maybe BranchName)
  | Stop (Maybe BranchName)
  | Status (Maybe BranchName)
  | Shell (Maybe BranchName)
  | Logs (Maybe BranchName)
  | List
  | Destroy (Maybe BranchName)

data CreateOpts = CreateOpts
  { createHost :: Text
  , createPort :: Int
  , createBranch :: Maybe BranchName
  , createConfig :: FilePath
  }

-- | Parse command line arguments
parseArgs :: Parser (GlobalOpts, Command)
parseArgs = (,) <$> globalOpts <*> commandParser
  where
    globalOpts = GlobalOpts
      <$> optional (strOption
          ( long "state-dir"
         <> metavar "DIR"
         <> help "Override default state directory" ))
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Enable verbose logging" )
      <*> switch
          ( long "debug"
         <> short 'd'
         <> help "Enable debug logging" )
      <*> option auto
          ( long "timeout"
         <> short 't'
         <> metavar "SECONDS"
         <> value 120
         <> help "Global timeout for VM operations" )

    commandParser = hsubparser
      ( command "create" (info createCmd (progDesc "Create a new VM"))
     <> command "start" (info startCmd (progDesc "Start VM"))
     <> command "stop" (info stopCmd (progDesc "Stop VM"))
     <> command "status" (info statusCmd (progDesc "Show VM status"))
     <> command "shell" (info shellCmd (progDesc "Open SSH shell"))
     <> command "logs" (info logsCmd (progDesc "Show VM logs"))
     <> command "list" (info (pure List) (progDesc "List all VMs"))
     <> command "destroy" (info destroyCmd (progDesc "Destroy VM"))
      )

    createCmd = Create <$> (CreateOpts
      <$> strOption (long "host" <> value "localhost" <> help "Host to bind VM ports")
      <*> option auto (long "port" <> value 8000 <> help "Port for MCP proxy")
      <*> optional (BranchName <$> strOption (long "branch" <> help "Branch name"))
      <*> strOption (long "config" <> value "vm-config.nix" <> help "VM config path"))

    startCmd = Start <$> optional (BranchName <$> argument str (metavar "BRANCH"))
    stopCmd = Stop <$> optional (BranchName <$> argument str (metavar "BRANCH"))
    statusCmd = Status <$> optional (BranchName <$> argument str (metavar "BRANCH"))
    shellCmd = Shell <$> optional (BranchName <$> argument str (metavar "BRANCH"))
    logsCmd = Logs <$> optional (BranchName <$> argument str (metavar "BRANCH"))
    destroyCmd = Destroy <$> optional (BranchName <$> argument str (metavar "BRANCH"))

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser opts
  result <- runVMCommand globalOpts cmd
  case result of
    Left err -> do
      putStrLn $ "Error: " <> show err
      exitFailure
    Right () -> return ()
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "VM control command for managing development VMs"
     <> header "agent-vm - Type-safe VM lifecycle management" )

-- | Execute VM command
runVMCommand :: GlobalOpts -> Command -> IO (Either VMError ())
runVMCommand opts cmd = do
  registry <- newVMRegistry
  let logger = vmLogger  -- TODO: Configure based on verbose/debug flags

  case cmd of
    Create createOpts -> undefined  -- TODO: Implement
    Start branch -> undefined       -- TODO: Implement
    Stop branch -> undefined        -- TODO: Implement
    Status branch -> undefined      -- TODO: Implement
    Shell branch -> undefined       -- TODO: Implement
    Logs branch -> undefined        -- TODO: Implement
    List -> undefined              -- TODO: Implement
    Destroy branch -> undefined     -- TODO: Implement
```

## Testing Infrastructure

### Integration Test Setup (`test/AgentVM/IntegrationSpec.hs`)
```haskell
{-# LANGUAGE OverloadedStrings #-}

module AgentVM.IntegrationSpec where

import Test.Hspec
import System.IO.Temp
import Control.Exception (bracket)
import AgentVM
import AgentVM.Types

spec :: Spec
spec = describe "Agent VM Integration Tests" $ do

  describe "VM Lifecycle" $ do
    it "completes full create-start-stop-destroy cycle" $ do
      pending  -- TODO: Implement

    it "handles VM restart correctly" $ do
      pending  -- TODO: Implement

    it "manages multiple VM instances" $ do
      pending  -- TODO: Implement

  describe "State Management" $ do
    it "persists configuration across restarts" $ do
      pending  -- TODO: Implement

    it "manages SSH keys correctly" $ do
      pending  -- TODO: Implement

    it "handles concurrent state updates" $ do
      pending  -- TODO: Implement

  describe "Network and Connectivity" $ do
    it "establishes SSH connection after VM start" $ do
      pending  -- TODO: Implement

    it "forwards MCP proxy port correctly" $ do
      pending  -- TODO: Implement

    it "handles port conflicts gracefully" $ do
      pending  -- TODO: Implement

  -- Add all 50 integration tests as specified...

-- | Test helper: Create temporary state directory
withTempStateDir :: (FilePath -> IO a) -> IO a
withTempStateDir = withSystemTempDirectory "agent-vm-test"

-- | Test helper: Create test VM configuration
testVMConfig :: BranchName -> VMConfig
testVMConfig branch = VMConfig
  { vmConfigHost = "localhost"
  , vmConfigPort = 8000
  , vmConfigSshPort = 2222
  , vmConfigMemory = 4096
  , vmConfigCores = 4
  , vmConfigWorkspace = "/tmp/test-workspace"
  , vmConfigNixPath = "vm-config.nix"
  }
```

## Implementation Phases

### Phase 1: Core Infrastructure (Weeks 1-2)
- [ ] Set up project with Cabal and haskell.nix
- [ ] Implement core types (Types.hs)
- [ ] Implement logging infrastructure (Logging.hs)
- [ ] Create STM-based registry (State.hs)
- [ ] Add JSON config parsing (Config.hs)

### Phase 2: VM Lifecycle Management (Weeks 3-4)
- [ ] Write comprehensive integration tests with `pending` bodies
- [ ] Implement Nix integration (Nix.hs)
- [ ] Add process management for VM startup
- [ ] Implement SSH connectivity (SSH.hs)
- [ ] Port all VM operations (create, start, stop, destroy)

### Phase 3: CLI Interface (Week 5)
- [ ] Write comprehensive CLI tests with `pending` bodies
- [ ] Implement optparse-applicative CLI (Main.hs)
- [ ] Add colored output using ansi-terminal
- [ ] Ensure proper error handling and reporting
- [ ] Maintain compatibility with Python CLI interface

### Phase 4: Advanced Features - agent-vmd systemd service (Weeks 6-7)
- [ ] Write comprehensive daemon tests with `pending` bodies
- [ ] Design systemd service architecture for host machine
- [ ] Implement supervisor trees using async/STM
- [ ] Add concurrent VM operation support
- [ ] Performance optimization and profiling

### Phase 5: Integration & Testing (Week 8)
- [ ] Complete all pending test implementations
- [ ] Documentation updates
- [ ] Performance benchmarking
- [ ] Create deployment strategy

## Development Guidelines

### Error Handling
- Use `Either VMError a` for operations that can fail
- Provide descriptive error messages
- Log all errors with appropriate severity

### Concurrency
- All shared state must go through STM
- Use async for long-running operations
- Implement proper cleanup with bracket patterns

### Testing
- Write tests first with `pending` implementations
- Aim for >90% code coverage
- Test both success and failure paths

### Performance
- Profile regularly during development
- Use ByteString for all I/O operations
- Minimize STM transaction scope
