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

import Protolude (Bool, Either, FilePath, IO, Int, Text, notImplemented)

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey = notImplemented

-- | Wait for SSH to become available
waitForSSH ::
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
