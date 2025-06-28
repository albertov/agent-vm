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

    -- * Path accessors
    vmDiskImage,
    vmSerialSocket,
    vmNixFile,
    vmConfigFile,
    vmGCRoot,
    vmStartScript,
    VMError (..),
  )
where

import Data.Aeson (FromJSON, ToJSON)
import Protolude hiding (group)
import System.Directory (getHomeDirectory)
import System.FilePath ((</>))

-- import UnliftIO (SomeException)

-- | VM states as type-level values
data VMState = Stopped | Starting | Running | Stopping | Failed

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
    stateDir :: FilePath
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

defVMConfig :: (MonadIO m) => Maybe FilePath -> Text -> FilePath -> m VMConfig
defVMConfig mStateDir name workspace = do
  defaultBaseStateDir <- (</> ".local/share/agent-vm") <$> liftIO getHomeDirectory
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
        stateDir = fromMaybe defaultBaseStateDir mStateDir </> toS name
      }

-- | Derived path functions from VMConfig stateDir
vmDiskImage :: VMConfig -> FilePath
vmDiskImage config = stateDir config </> "disk.qcow2"

-- | Path to vm.nix file
vmNixFile :: VMConfig -> FilePath
vmNixFile config = stateDir config </> "vm.nix"

-- | Path to config.json file
vmConfigFile :: VMConfig -> FilePath
vmConfigFile config = stateDir config </> "config.json"

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

-- | Errors that can occur during VM operations
data VMError
  = VMNotFound FilePath
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
