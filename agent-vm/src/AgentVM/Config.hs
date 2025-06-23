{-# LANGUAGE DeriveGeneric #-}

-- | Configuration management for agent-vm
module AgentVM.Config
  ( VMConfigJson(..)
  , loadVMConfig
  , saveVMConfig
  ) where

import AgentVM.Types
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
loadVMConfig = error "loadVMConfig has not been implemented yet"

-- | Save VM configuration to state directory
saveVMConfig :: FilePath -> BranchName -> VMConfigJson -> IO ()
saveVMConfig = error "saveVMConfig has not been implemented yet"
