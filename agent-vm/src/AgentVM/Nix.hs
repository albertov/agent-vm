{-# LANGUAGE OverloadedStrings #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( buildVMConfig,
    runVMScript,
  )
where

import AgentVM.Log (AgentVmTrace (NixBuildCompleted, NixBuildFailed, NixBuildStarted, ProcessSpawned), (<&))
import AgentVM.Types (BranchName, VMError (NixBuildFailed), unBranchName)
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import Plow.Logging (IOTracer (IOTracer))
import Protolude (Either (Left, Right), FilePath, IO, return, ($), (<>))
import System.Process.Typed (ExitCode (ExitFailure, ExitSuccess), Process, byteStringOutput, closed, getStderr, getStdout, nullStream, proc, setStderr, setStdin, setStdout, startProcess, waitExitCode, withProcessWait)

-- | Build VM configuration using Nix
buildVMConfig :: IOTracer AgentVmTrace -> BranchName -> FilePath -> IO (Either VMError FilePath)
buildVMConfig (IOTracer logger) branchName workspace = do
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
runVMScript :: IOTracer AgentVmTrace -> FilePath -> IO (Process () () ())
runVMScript (IOTracer logger) scriptPath = do
  let procConfig =
        setStdin closed $
          setStdout nullStream $
            setStderr nullStream $
              proc scriptPath []

  logger <& ProcessSpawned (T.pack scriptPath) []
  startProcess procConfig
