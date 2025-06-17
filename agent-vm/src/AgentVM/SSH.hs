{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | SSH management for VMs
module AgentVM.SSH
  ( generateSSHKey,
    waitForSSH,
    sshExec,
    trySSHConnect,
  )
where

import AgentVM.Log (AgentVmTrace)
import Data.Generics.Product (HasType, the)
import Lens.Micro.Mtl (view)
import Plow.Logging (IOTracer, traceWith)
import Protolude

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey = notImplemented

-- | Wait for SSH to become available
waitForSSH ::
  ( MonadReader env m,
    HasType (IOTracer AgentVmTrace) env,
    MonadIO m
  ) =>
  Text ->
  Int ->
  FilePath ->
  Int ->
  m Bool
waitForSSH = notImplemented

-- | Execute command over SSH
sshExec :: Text -> Int -> FilePath -> Text -> IO (Either Text Text)
sshExec = notImplemented

-- | Try to establish SSH connection
trySSHConnect :: Text -> Int -> FilePath -> IO (Either Text ())
trySSHConnect = notImplemented
