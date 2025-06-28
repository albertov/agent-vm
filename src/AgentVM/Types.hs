{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Core type definitions for agent-vm
module AgentVM.Types
  ( -- * VM States
    VMState (..),
    VMConfig (..),
    defVMConfig,

    -- * VM Status Types
    VMStatus (..),
    MemoryInfo (..),
    CPUInfo (..),

    -- * Path accessors
    vmDiskImage,
    vmSerialSocket,
    vmNixFile,
    vmConfigFile,
    vmConfigFile2,
    vmGCRoot,
    vmStartScript,
    vmPidFile,
    VMError (..),
    getDefaultBaseStateDir,
  )
where

import AgentVM.Git (GitInfo)
import Data.Aeson (FromJSON, ToJSON)
import Data.Time.Clock (NominalDiffTime)
import Protolude hiding (group)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- import UnliftIO (SomeException)

-- | VM states as type-level values
data VMState = Stopped | Starting | Running VMStatus | Stopping | Failed

-- | Detailed VM status information
data VMStatus = VMStatus
  { vmStatusPid :: Int,
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

-- | VM configuration matching vm-base.nix module options
data VMConfig = VMConfig
  { name :: Text,
    tmpfs :: Bool,
    memorySize :: Int, -- In GB
    cores :: Int,
    diskSize :: Int, -- In GB
    additionalPaths :: [Text], -- Package names
    workspace :: FilePath, -- Required - no default in Nix
    port :: Int,
    systemPackages :: [Text], -- Package names
    uid :: Int,
    gid :: Int,
    group :: Text,
    -- | This is the name of the shell in the flake that we want to inject.
    -- Defaults to "default"
    shellName :: Text,
    -- | This is the flake where the development shell we want to inject in the
    -- VM lives in
    flake :: Text,
    nixBaseConfig :: Maybe FilePath,
    stateDir :: FilePath,
    -- | Shell escape key sequence for exiting interactive shell sessions.
    -- Defaults to "Ctrl-W"
    shellEscapeKey :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

getDefaultBaseStateDir :: (MonadIO m) => m FilePath
getDefaultBaseStateDir = (</> ".local/share/agent-vm") <$> liftIO getHomeDirectory

defVMConfig :: (MonadIO m) => Maybe FilePath -> Text -> FilePath -> m VMConfig
defVMConfig mStateDir name workspace = do
  vmStateDir' <- vmStateDir mStateDir name
  pure
    VMConfig
      { tmpfs = True,
        memorySize = 4,
        cores = 2,
        diskSize = 4,
        additionalPaths = [],
        name,
        workspace,
        port = 8000,
        systemPackages = ["vim", "git"],
        uid = 1000,
        gid = 1000,
        group = "mcp-proxy",
        shellName = "default",
        flake = ".",
        nixBaseConfig = Nothing,
        stateDir = toS vmStateDir',
        shellEscapeKey = "Ctrl-W"
      }

-- | Computes VM state dir from global statedir and VM name
vmStateDir :: (MonadIO m) => Maybe FilePath -> Text -> m FilePath
vmStateDir mStateDir name = do
  defaultBaseStateDir <- getDefaultBaseStateDir
  pure $ fromMaybe defaultBaseStateDir mStateDir </> toS name

-- | Derived path functions from VMConfig stateDir
vmDiskImage :: VMConfig -> FilePath
vmDiskImage config = stateDir config </> "disk.qcow2"

-- | Path to vm.nix file
vmNixFile :: VMConfig -> FilePath
vmNixFile config = stateDir config </> "vm.nix"

-- | Path to config.json file
vmConfigFile :: VMConfig -> FilePath
vmConfigFile config = stateDir config </> "config.json"

vmConfigFile2 :: (MonadIO m) => Maybe FilePath -> Text -> m FilePath
vmConfigFile2 mStateDir = fmap (</> "config.json") . vmStateDir mStateDir

-- | Path to gc-root symlink
vmGCRoot :: VMConfig -> FilePath
vmGCRoot config = stateDir config </> "vm-start"

-- | Path to the script that starts the vm and allows us to communicate with a
-- login shell at the virtual qemu terminal
vmStartScript :: VMConfig -> FilePath
vmStartScript config = vmGCRoot config </> "bin" </> "run-" <> toS (name config) <> "-vm-virtiofs"

-- | Path to the serial console socket
vmSerialSocket :: VMConfig -> FilePath
vmSerialSocket config = stateDir config </> "serial.sock"

-- | Path to the pid file
vmPidFile :: VMConfig -> FilePath
vmPidFile config = stateDir config </> "vm.pid"

-- | Errors that can occur during VM operations
data VMError
  = VMNotFound FilePath
  | ConfigError Text
  | WorkspaceError Text
  | CommandTimeout
  -- TODO: So the runVMT version can catch
  -- unhandled exceptions with tryAny

  deriving
    ( -- | UnhandledException SomeException
      Eq,
      Show,
      Exception
    )
