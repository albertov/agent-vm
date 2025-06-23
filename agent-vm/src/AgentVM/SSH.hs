-- | SSH management for VMs
module AgentVM.SSH
  ( generateSSHKey
  , waitForSSH
  , sshExec
  , trySSHConnect
  ) where

import AgentVM.Types
import AgentVM.Log
import Data.Text (Text)
import System.Exit (ExitCode(..))

-- | Generate SSH keypair
generateSSHKey :: FilePath -> IO ()
generateSSHKey = error "generateSSHKey has not been implemented yet"

-- | Wait for SSH to become available
waitForSSH :: LogAction IO AgentVmTrace -> Text -> Int -> FilePath -> Int -> IO Bool
waitForSSH = error "waitForSSH has not been implemented yet"

-- | Execute command over SSH
sshExec :: Text -> Int -> FilePath -> Text -> IO (Either Text Text)
sshExec = error "sshExec has not been implemented yet"

-- | Try to establish SSH connection
trySSHConnect :: Text -> Int -> FilePath -> IO (Either Text ())
trySSHConnect = error "trySSHConnect has not been implemented yet"
