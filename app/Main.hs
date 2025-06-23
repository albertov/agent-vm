{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Main entry point for agent-vm CLI
module Main (main) where

import AgentVM.Log (AgentVmTrace (ProcessError), LogLevel, renderLogLevel, renderTrace, traceLevel, vmLogger)
import Data.Functor.Contravariant (contramap)
import Options.Applicative
  ( Parser,
    auto,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    option,
    optional,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<**>),
  )
import Plow.Logging (traceWith)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude
import System.Exit (exitFailure)
import System.IO (stderr)

data GlobalOpts = GlobalOpts
  { optStateDir :: Maybe FilePath,
    optVerbose :: Bool,
    optDebug :: Bool,
    optTimeout :: Int
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
  deriving (Show)

-- | Parse command line arguments
parseArgs :: Parser (GlobalOpts, Command)
parseArgs = (,) <$> globalOpts <*> commandParser
  where
    globalOpts =
      GlobalOpts
        <$> optional
          ( strOption
              ( long "state-dir"
                  <> metavar "DIR"
                  <> help "Override default state directory"
              )
          )
        <*> switch
          ( long "verbose"
              <> short 'v'
              <> help "Enable verbose logging"
          )
        <*> switch
          ( long "debug"
              <> short 'd'
              <> help "Enable debug logging"
          )
        <*> option
          auto
          ( long "timeout"
              <> short 't'
              <> metavar "SECONDS"
              <> value 120
              <> help "Global timeout for VM operations"
          )

    commandParser =
      hsubparser
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
  (_globalOpts, cmd) <- execParser opts
  -- Set up async logging with a buffer of 1000 messages
  withAsyncHandleTracer stderr 1000 $ \asyncTracer -> do
    -- Create the final tracer that converts AgentVmTrace to Text
    let finalTracer = contramap (renderLogLevel renderTrace . traceLevel) asyncTracer

    -- For now, just demonstrate logging is working
    traceWith finalTracer $ ProcessError "agent-vm" ("Haskell agent-vm: " <> toS (show cmd :: [Char]) <> " (not yet implemented)")
    exitFailure
  where
    opts =
      info
        (parseArgs <**> helper)
        ( fullDesc
            <> progDesc "VM control command for managing development VMs"
            <> header "agent-vm - Type-safe VM lifecycle management"
        )
