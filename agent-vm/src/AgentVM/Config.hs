{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | Configuration management for agent-vm
module AgentVM.Config
  ( VMConfigJson(..)
  , loadVMConfig
  , saveVMConfig
  , vmConfigFromJson
  , vmConfigToJson
  ) where

import AgentVM.Types
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import GHC.Generics
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing)

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
  } deriving (Generic, Show, Eq)

instance FromJSON VMConfigJson where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance ToJSON VMConfigJson where
  toJSON = genericToJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

-- | Convert from JSON config to VM config
vmConfigFromJson :: VMConfigJson -> BranchName -> VMConfig
vmConfigFromJson json _ = VMConfig
  { vmConfigHost = host json
  , vmConfigPort = port json
  , vmConfigSshPort = ssh_port json
  , vmConfigMemory = 4096  -- Default values
  , vmConfigCores = 4      -- Default values
  , vmConfigWorkspace = workspace_path json
  , vmConfigNixPath = nix_config_path json
  }

-- | Convert from VM config to JSON config (for saving)
vmConfigToJson :: BranchName -> VMConfig -> UTCTime -> FilePath -> Maybe Text -> (Int, Int) -> VMConfigJson
vmConfigToJson branch config createdAt sshKeyPath upstreamRepo (uid, gid) = VMConfigJson
  { vm_name = unBranchName branch
  , host = vmConfigHost config
  , port = vmConfigPort config
  , ssh_port = vmConfigSshPort config
  , ssh_key_path = sshKeyPath
  , workspace_path = vmConfigWorkspace config
  , nix_config_path = vmConfigNixPath config
  , created_at = createdAt
  , upstream_repo = upstreamRepo
  , host_uid = uid
  , host_gid = gid
  }

-- | Load VM configuration from state directory
loadVMConfig :: FilePath -> BranchName -> IO (Either String VMConfigJson)
loadVMConfig stateDir branch = do
  let configPath = stateDir </> T.unpack (unBranchName branch) </> "config.json"
  eitherDecodeFileStrict' configPath

-- | Save VM configuration to state directory
saveVMConfig :: FilePath -> BranchName -> VMConfigJson -> IO ()
saveVMConfig stateDir branch config = do
  let branchDir = stateDir </> T.unpack (unBranchName branch)
      configPath = branchDir </> "config.json"
  createDirectoryIfMissing True branchDir
  encodeFile configPath config
