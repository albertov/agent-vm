-- | SSH management for VMs
module AgentVM.SSH
  ( generateSSHKey,
    waitForSSH,
    sshExec,
    trySSHConnect,
  )
where

import AgentVM.Log (AgentVmTrace)
import Plow.Logging (IOTracer)
import Protolude (Bool, Either, FilePath, IO, Int, Text, notImplemented)

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey = notImplemented

-- | Wait for SSH to become available
waitForSSH :: IOTracer AgentVmTrace -> Text -> Int -> FilePath -> Int -> IO Bool
waitForSSH = notImplemented

-- | Execute command over SSH
sshExec :: Text -> Int -> FilePath -> Text -> IO (Either Text Text)
sshExec = notImplemented

-- | Try to establish SSH connection
trySSHConnect :: Text -> Int -> FilePath -> IO (Either Text ())
trySSHConnect = notImplemented
