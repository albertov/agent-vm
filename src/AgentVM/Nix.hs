{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Nix integration for VM management
module AgentVM.Nix
  ( saveVMConfig,
    loadVMConfig,
    buildVMImage,
  )
where

import AgentVM.Flake (getFlakeReference)
import AgentVM.Log (AgentVmTrace (..), MonadTrace (trace))
import AgentVM.Types
  ( VMConfig (..),
    VMError (CommandTimeout),
    vmConfigFile,
    vmGCRoot,
    vmNixFile,
    vmSerialSocket,
  )
import Control.Concurrent.Timeout (timeout)
import Data.Aeson (decode, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Language.Haskell.TH as TH (runIO)
import qualified Language.Haskell.TH.Syntax as TH (lift)
import NeatInterpolation (text)
import Protolude hiding (group, trace, try)
import qualified Shelly as Sh
import System.Directory
  ( createDirectoryIfMissing,
    doesFileExist,
  )
import System.Environment (lookupEnv)
import UnliftIO (MonadUnliftIO, hClose, withSystemTempFile)

-- | Calculate shell environment and save as gc-root
calculateShellEnv ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m FilePath
calculateShellEnv vmConfig = do
  let flakeRef = flake vmConfig
      devShell = shellName vmConfig
      shellCommand = ["--accept-flake-config", "print-dev-env", flakeRef <> "#" <> devShell]

  trace $ NixBuildStarted ("Calculating shell environment for " <> flakeRef <> "#" <> devShell)

  shellEnv <- runSh $ Sh.silently $ Sh.run "nix" shellCommand
  -- Write shell env to temporary file using writeFile for proper handling
  withSystemTempFile "shell-env" $ \shellEnvTmpPath fileHandle -> do
    hPutStr fileHandle shellEnv
    hClose fileHandle
    nixStoreAdd (toS shellEnvTmpPath)

nixStoreAdd :: (MonadIO m) => FilePath -> m FilePath
nixStoreAdd path =
  fmap toS $ runSh $ Sh.silently $ Sh.run "nix-store" ["--add", toS path]

-- | Build the VM nix configuration file
buildVMNixFile ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  FilePath ->
  m FilePath
buildVMNixFile vmConfig shellEnvPath = do
  nixContent <- generateVMNixContent vmConfig shellEnvPath
  let nixFilePath = vmNixFile vmConfig
  liftIO $ writeFile nixFilePath nixContent
  trace $ NixBuildStarted ("Generated VM configuration: " <> toS nixFilePath)
  pure nixFilePath

-- | Generate the content for the VM nix file
generateVMNixContent :: (MonadTrace AgentVmTrace m, MonadIO m) => VMConfig -> FilePath -> m Text
generateVMNixContent vmConfig (toS -> shellEnvPath) = do
  saveVMConfig vmConfig
  pure
    [text|
{ pkgs, lib, ... }:
{
  imports = [ $nixBaseConfigValue  ];

  agent-vm =
    let
       resolvePackages = builtins.map (p: pkgs."$${p}");
       mapAttr = name:
         { additionalPaths = resolvePackages;
           systemPackages = resolvePackages;
         }."$${name}" or lib.mkDefault;

    in { shellEnv = $shellEnvPath; serialSocket = "$serialSocket"; }
    // (builtins.mapAttrs mapAttr
         (builtins.removeAttrs
            (builtins.fromJSON (builtins.readFile $configPath))
            ["name" "flake" "shellName" "nixBaseConfig" "stateDir"]));
}
|]
  where
    nixBaseConfigValue = maybe "" toS (nixBaseConfig vmConfig)
    configPath = toS (vmConfigFile vmConfig)
    serialSocket = toS (vmSerialSocket vmConfig)

-- | Build the VM image and save gc-root
buildVMImage ::
  ( MonadTrace AgentVmTrace m,
    MonadUnliftIO m
  ) =>
  VMConfig ->
  m ()
buildVMImage vmConfig = do
  let vmDirPath = stateDir vmConfig
  liftIO $ createDirectoryIfMissing True vmDirPath
  trace $ WorkspaceCreated vmDirPath
  shellEnvPath <- calculateShellEnv vmConfig
  nixFilePath <- buildVMNixFile vmConfig shellEnvPath
  let gcRootPath = vmGCRoot vmConfig
      nixFilePathVar = toS nixFilePath
  selfRef <- maybe getFlakeReference pure $(TH.runIO (TH.lift . fmap (toS @_ @Text) =<< lookupEnv "AGENT_VM_SELF"))
  let buildExpression =
        [text|
        let self = builtins.getFlake "$selfRef";
            system = builtins.currentSystem;
            mkVM = mods: (self.lib."$${system}".mk-agent-vm mods).config.system.build.vmWithVirtioFS;
        in mkVM [ $nixFilePathVar ]
      |]
  trace $ NixBuildStarted "Building VM image"
  runSh $
    Sh.run_
      "nix"
      [ "build",
        "--accept-flake-config",
        "--show-trace",
        "--impure",
        "--expr",
        toS buildExpression,
        "-o",
        toS gcRootPath
      ]

-- | Save VM configuration to JSON file
saveVMConfig :: (MonadIO m) => VMConfig -> m ()
saveVMConfig config = do
  liftIO $ LBS.writeFile (vmConfigFile config) (encode config)

-- | Load VM configuration from JSON file
loadVMConfig :: (MonadIO m) => FilePath -> m (Maybe VMConfig)
loadVMConfig configFilePath = do
  exists <- liftIO $ doesFileExist configFilePath
  if exists
    then do
      content <- liftIO $ LBS.readFile configFilePath
      pure $ decode content
    else pure Nothing

runSh :: (MonadIO m) => Sh.Sh a -> m a
runSh = maybe (throwIO CommandTimeout) pure <=< liftIO . timeout defTimeout . Sh.shelly

defTimeout :: Integer
defTimeout = 1800_000_000
