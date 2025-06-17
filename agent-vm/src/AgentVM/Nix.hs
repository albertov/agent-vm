{-# LANGUAGE OverloadedStrings #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig
  , runVMScript
  ) where

import AgentVM.Types
import AgentVM.Log
import System.Process.Typed
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSL8
import System.Exit (ExitCode(..))
import Control.Concurrent.STM (atomically)

-- | Build VM configuration using Nix
buildVMConfig :: LogAction IO AgentVmTrace -> BranchName -> FilePath -> IO (Either VMError FilePath)
buildVMConfig logger branch workspace = do
  logger <& NixBuildStarted flakeRef

  let procConfig = setStdin closed
                 $ setStdout byteStringOutput
                 $ setStderr byteStringOutput
                 $ proc "nix" ["build", T.unpack flakeRef, "--no-link", "--print-out-paths"]

  withProcessWait procConfig $ \p -> do
    exitCode <- waitExitCode p
    stdout <- atomically $ getStdout p
    stderr <- atomically $ getStderr p

    case exitCode of
      ExitSuccess -> do
        let storePath = T.strip $ T.pack $ BSL8.unpack stdout
        logger <& NixBuildCompleted (T.unpack storePath)
        return $ Right (T.unpack storePath)
      ExitFailure _ -> do
        let errorMsg = T.pack $ BSL8.unpack stderr
        logger <& AgentVM.Log.NixBuildFailed errorMsg
        return $ Left (AgentVM.Types.NixBuildFailed errorMsg)
  where
    flakeRef = "path:" <> T.pack workspace <> "#vm-config." <> unBranchName branch

-- | Run Nix-generated VM script
runVMScript :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
runVMScript logger scriptPath = do
  let procConfig = setStdin closed
                 $ setStdout createPipe
                 $ setStderr createPipe
                 $ proc scriptPath []

  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess procConfig
