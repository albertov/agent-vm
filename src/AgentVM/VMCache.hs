{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Cache for VM workspace-to-name mappings
module AgentVM.VMCache
  ( -- * Types
    VMCache (..),

    -- * Operations
    loadVMCache,
    saveVMCache,
    addVMToCache,
    removeVMFromCache,
    lookupVMByWorkspace,
    getVMCacheFile,
  )
where

import Data.Aeson (FromJSON, ToJSON, eitherDecodeFileStrict', encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as Map
import qualified Data.Text.Encoding as TE
import Protolude
import System.AtomicWrite.Writer.Text (atomicWriteFile)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import UnliftIO (MonadUnliftIO, tryAny)

-- | Map from workspace path to VM name
newtype VMCache = VMCache {unVMCache :: Map FilePath Text}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Get the path to the vms.json file
getVMCacheFile :: Maybe FilePath -> IO FilePath
getVMCacheFile mStateDir = do
  baseStateDir <- case mStateDir of
    Just dir -> pure dir
    Nothing -> (</> ".local/share/agent-vm") <$> getHomeDirectory
  pure $ baseStateDir </> "vms.json"

-- | Load the VM cache from disk
loadVMCache :: (MonadUnliftIO m) => Maybe FilePath -> m VMCache
loadVMCache mStateDir = do
  cacheFile <- liftIO $ getVMCacheFile mStateDir
  exists <- liftIO $ doesFileExist cacheFile
  if exists
    then do
      result <- liftIO $ eitherDecodeFileStrict' cacheFile
      case result of
        Left _ -> do
          -- If we can't parse the file, start with empty cache
          -- Log error if we have logging available
          pure $ VMCache Map.empty
        Right cache -> pure cache
    else pure $ VMCache Map.empty

-- | Save the VM cache to disk atomically
saveVMCache :: (MonadUnliftIO m) => Maybe FilePath -> VMCache -> m ()
saveVMCache mStateDir cache = do
  cacheFile <- liftIO $ getVMCacheFile mStateDir
  let jsonBytes = encode cache
      jsonText = case TE.decodeUtf8' $ LBS.toStrict jsonBytes of
        Left _ -> panic "Failed to decode JSON as UTF-8"
        Right txt -> txt
  -- Use atomic write to prevent corruption
  void $ tryAny $ liftIO $ atomicWriteFile cacheFile jsonText

-- | Add a VM to the cache
addVMToCache :: (MonadUnliftIO m) => Maybe FilePath -> FilePath -> Text -> m ()
addVMToCache mStateDir workspace vmName = do
  cache <- loadVMCache mStateDir
  let newCache = VMCache $ Map.insert workspace vmName (unVMCache cache)
  saveVMCache mStateDir newCache

-- | Remove a VM from the cache
removeVMFromCache :: (MonadUnliftIO m) => Maybe FilePath -> FilePath -> m ()
removeVMFromCache mStateDir workspace = do
  cache <- loadVMCache mStateDir
  let newCache = VMCache $ Map.delete workspace (unVMCache cache)
  saveVMCache mStateDir newCache

-- | Look up a VM name by workspace path
-- Returns Nothing if the cache doesn't exist, can't be read, or doesn't contain the workspace.
-- This allows graceful fallback to other name resolution methods.
lookupVMByWorkspace :: (MonadUnliftIO m) => Maybe FilePath -> FilePath -> m (Maybe Text)
lookupVMByWorkspace mStateDir workspace = do
  cache <- loadVMCache mStateDir
  pure $ Map.lookup workspace (unVMCache cache)
