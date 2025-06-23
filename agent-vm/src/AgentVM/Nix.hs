{-# LANGUAGE OverloadedStrings #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig,
    runVMScript,
  )
where

import AgentVM.Log (AgentVmTrace (NixBuildCompleted, NixBuildFailed, NixBuildProgress, NixBuildStarted, ProcessSpawned), LogAction, (<&))
import AgentVM.Types (BranchName, VMError (NixBuildFailed), unBranchName)
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Protolude (Either (Left, Right), FilePath, IO, pure, return, ($), (<>))
import System.Process.Typed (ExitCode (..), Process, byteStringOutput, closed, getStderr, getStdout, nullStream, proc, readProcess, setStderr, setStdin, setStdout, startProcess, waitExitCode, withProcessWait)

-- | Build VM configuration using Nix
buildVMConfig :: LogAction IO AgentVmTrace -> BranchName -> FilePath -> IO (Either VMError FilePath)
buildVMConfig logger branchName workspace = do
  logger <& NixBuildStarted flakeRefText

  let procConfig =
        setStdin closed $
          setStdout byteStringOutput $
            setStderr byteStringOutput $
              proc "nix" ["build", T.unpack flakeRefText, "--no-link", "--print-out-paths"]

  withProcessWait procConfig $ \p -> do
    processExitCode <- waitExitCode p
    stdout <- atomically $ getStdout p
    stderr <- atomically $ getStderr p

    case processExitCode of
      ExitSuccess -> do
        let vmStorePath = T.strip $ T.pack $ BSL8.unpack stdout
        logger <& NixBuildCompleted (T.unpack vmStorePath)
        return $ Right (T.unpack vmStorePath)
      ExitFailure _ -> do
        let nixErrorMsg = T.pack $ BSL8.unpack stderr
        logger <& AgentVM.Log.NixBuildFailed nixErrorMsg
        return $ Left (AgentVM.Types.NixBuildFailed nixErrorMsg)
  where
    flakeRefText = "path:" <> T.pack workspace <> "#vm-config." <> unBranchName branchName

-- | Run Nix-generated VM script
runVMScript :: LogAction IO AgentVmTrace -> FilePath -> IO (Process () () ())
runVMScript logger scriptPath = do
  let procConfig =
        setStdin closed $
          setStdout nullStream $
            setStderr nullStream $
              proc scriptPath []

  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess procConfig
