-- | SSH management for VMs
module AgentVM.SSH
  ( generateSSHKey
  , waitForSSH
  , sshExec
  , trySSHConnect
  ) where

import Protolude (IO, FilePath, Text, Int, Bool, Either, notImplemented)

import AgentVM.Log (AgentVmTrace, LogAction)

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey = notImplemented

-- | Wait for SSH to become available
waitForSSH :: LogAction IO AgentVmTrace -> Text -> Int -> FilePath -> Int -> IO Bool
waitForSSH = notImplemented

-- | Execute command over SSH
sshExec :: Text -> Int -> FilePath -> Text -> IO (Either Text Text)
sshExec = notImplemented

-- | Try to establish SSH connection
trySSHConnect :: Text -> Int -> FilePath -> IO (Either Text ())
trySSHConnect = notImplemented
