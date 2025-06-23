-- | Main entry point for agent-vm CLI
module Main where

import Options.Applicative
import System.Exit (exitFailure, exitSuccess)

data GlobalOpts = GlobalOpts
  { optStateDir :: Maybe FilePath
  , optVerbose :: Bool
  , optDebug :: Bool
  , optTimeout :: Int
  }

data Command
  = Create
  | Start
  | Stop
  | Status
  | Shell
  | Logs
  | List
  | Destroy

-- | Parse command line arguments
parseArgs :: Parser (GlobalOpts, Command)
parseArgs = (,) <$> globalOpts <*> commandParser
  where
    globalOpts = GlobalOpts
      <$> optional (strOption
          ( long "state-dir"
         <> metavar "DIR"
         <> help "Override default state directory" ))
      <*> switch
          ( long "verbose"
         <> short 'v'
         <> help "Enable verbose logging" )
      <*> switch
          ( long "debug"
         <> short 'd'
         <> help "Enable debug logging" )
      <*> option auto
          ( long "timeout"
         <> short 't'
         <> metavar "SECONDS"
         <> value 120
         <> help "Global timeout for VM operations" )

    commandParser = hsubparser
      ( command "create" (info (pure Create) (progDesc "Create a new VM"))
     <> command "start" (info (pure Start) (progDesc "Start VM"))
     <> command "stop" (info (pure Stop) (progDesc "Stop VM"))
     <> command "status" (info (pure Status) (progDesc "Show VM status"))
     <> command "shell" (info (pure Shell) (progDesc "Open SSH shell"))
     <> command "logs" (info (pure Logs) (progDesc "Show VM logs"))
     <> command "list" (info (pure List) (progDesc "List all VMs"))
     <> command "destroy" (info (pure Destroy) (progDesc "Destroy VM"))
      )

main :: IO ()
main = do
  (globalOpts, cmd) <- execParser opts
  putStrLn $ "Haskell agent-vm: " ++ show cmd ++ " (not yet implemented)"
  exitFailure
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "VM control command for managing development VMs"
     <> header "agent-vm - Type-safe VM lifecycle management" )
