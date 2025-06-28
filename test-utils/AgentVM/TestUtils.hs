{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

-- | Test utilities for AgentVM testing
module AgentVM.TestUtils
  ( withTestEnv,
    withGitWorkspaceTestEnv,
    parseLogLevel, -- Export for testing
    eventually,
    createBaseVMFixture,
    createFlakeFixture,
    createShellFixture,
  )
where

import AgentVM.Env (AgentVmEnv (..))
import AgentVM.Git (gitInit)
import AgentVM.Log (AgentVmTrace, LogLevel (..), renderTracedMessage, traceLevel)
import AgentVM.Types (VMConfig (..), defVMConfig)
import Data.Generics.Labels ()
import qualified Data.Text as T
import Lens.Micro ((.~), (^.))
import Plow.Logging (IOTracer (IOTracer), Tracer (Tracer), filterTracer)
import Plow.Logging.Async (withAsyncHandleTracer)
import Protolude hiding (Handler)
import qualified Shelly as Sh
import System.Directory (createDirectoryIfMissing)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.Random (randomRIO)
import Test.HUnit.Lang (HUnitFailure)
import UnliftIO (Handler (Handler), MonadUnliftIO, withSystemTempDirectory)
import UnliftIO.IORef (IORef, atomicModifyIORef', newIORef)
import UnliftIO.Retry (fullJitterBackoff, limitRetriesByCumulativeDelay, recovering)

-- | Parse LogLevel from string using Read instance
parseLogLevel :: Text -> Maybe LogLevel
parseLogLevel = readMaybe

withTestEnv ::
  (MonadUnliftIO m) =>
  ((AgentVmEnv, IORef [AgentVmTrace]) -> m a) ->
  m a
withTestEnv fun = do
  -- Read minimum log level from environment variable
  minLevelEnv <- liftIO $ lookupEnv "AGENT_VM_LOGLEVEL"
  minLevel <- case minLevelEnv of
    Just levelStr -> case readMaybe levelStr of
      Just logLevel -> pure logLevel
      Nothing -> liftIO $ die $ "Invalid logLevel: " <> toS levelStr
    Nothing -> pure Info
  withAsyncHandleTracer stderr 1000 $ \asyncTracer ->
    withSystemTempDirectory "agent-vm-tests-XXXXXXX" $ \stateDir -> do
      tracesRef <- newIORef []
      let refTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          refTracer =
            Tracer $
              \traceEvent -> liftIO $ atomicModifyIORef' tracesRef ((,()) . (traceEvent :))

          vmTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          vmTracer = filterTracer (\traceEvent -> traceLevel traceEvent >= minLevel) $
            Tracer $ \traceEvent -> do
              let IOTracer (Tracer textTracerFunc) = asyncTracer
              textTracerFunc (renderTracedMessage traceEvent)

          tracer = IOTracer $ refTracer <> vmTracer
      vmConfig <-
        defVMConfig (Just stateDir) "test" (stateDir </> "workspaceDir")
          <&> #group
            .~ "users" -- FIXME Un-hardcode, fetch from environment
      fun (AgentVmEnv {tracer, vmConfig}, tracesRef)

-- | Retry an assertion several times until a timeout expires
eventually :: (MonadUnliftIO m) => m a -> m a
eventually =
  recovering
    (limitRetriesByCumulativeDelay 30_000_000 (fullJitterBackoff 10))
    [ const (Handler (\(_ :: HUnitFailure) -> pure True)),
      const (Handler (\(_ :: SomeException) -> pure False))
    ]
    . const

-- | Create a test environment with a git workspace
withGitWorkspaceTestEnv ::
  (MonadUnliftIO m) =>
  ((AgentVmEnv, IORef [AgentVmTrace]) -> m a) ->
  m a
withGitWorkspaceTestEnv fun = do
  -- Generate a unique VM name for this test
  randomSuffix <- liftIO $ randomRIO (1000, 9999 :: Int)
  let uniqueVmName = "test-vm-" <> T.pack (show randomSuffix)

  -- Read minimum log level from environment variable
  minLevelEnv <- liftIO $ lookupEnv "AGENT_VM_LOGLEVEL"
  minLevel <- case minLevelEnv of
    Just levelStr -> case readMaybe levelStr of
      Just logLevel -> pure logLevel
      Nothing -> liftIO $ die $ "Invalid logLevel: " <> toS levelStr
    Nothing -> pure Info
  withAsyncHandleTracer stderr 1000 $ \asyncTracer ->
    withSystemTempDirectory "agent-vm-tests-XXXXXXX" $ \stateDir -> do
      tracesRef <- newIORef []
      let refTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          refTracer =
            Tracer $
              \traceEvent -> liftIO $ atomicModifyIORef' tracesRef ((,()) . (traceEvent :))

          vmTracer :: forall m. (MonadIO m) => Tracer m AgentVmTrace
          vmTracer = filterTracer (\traceEvent -> traceLevel traceEvent >= minLevel) $
            Tracer $ \traceEvent -> do
              let IOTracer (Tracer textTracerFunc) = asyncTracer
              textTracerFunc (renderTracedMessage traceEvent)

          tracer = IOTracer $ refTracer <> vmTracer
      vmConfig <-
        defVMConfig (Just stateDir) uniqueVmName (stateDir </> "workspaceDir")
          <&> #group
            .~ "users" -- FIXME Un-hardcode, fetch from environment
      let env = AgentVmEnv {tracer, vmConfig}
          workspaceDir = vmConfig ^. #workspace

      -- Create workspace directory and initialize git repository
      liftIO $ do
        createDirectoryIfMissing True workspaceDir
        gitInit workspaceDir

      -- Create fixtures directory
      let fixturesDir = stateDir </> "fixtures"
      liftIO $ createDirectoryIfMissing True fixturesDir

      -- Create base VM fixture
      baseFixture <- liftIO $ createBaseVMFixture fixturesDir

      -- Create flake fixture
      flakeFixture <- liftIO $ createFlakeFixture fixturesDir

      -- Update vmConfig with fixture paths
      let updatedVmConfig =
            vmConfig
              { nixBaseConfig = Just baseFixture,
                flake = toS flakeFixture
              }
          updatedEnv = env {vmConfig = updatedVmConfig}

      fun (updatedEnv, tracesRef)

-- | Create a base VM configuration fixture
createBaseVMFixture :: FilePath -> IO FilePath
createBaseVMFixture fixturesDir = do
  let baseVmFile = fixturesDir </> "base-vm.nix"
  writeFile baseVmFile $
    unlines
      [ "{ config, lib, pkgs, ... }:",
        "{",
        "  # Test base VM configuration",
        "  environment.systemPackages = with pkgs; [",
        "    vim",
        "    git",
        "  ];",
        "",
        "  services.openssh.enable = true;",
        "}"
      ]
  pure baseVmFile

-- | Create a flake fixture for development shells
createFlakeFixture :: FilePath -> IO FilePath
createFlakeFixture fixturesDir = do
  let flakeDir = fixturesDir </> "test-flake"
  createDirectoryIfMissing True flakeDir

  -- Create flake.nix
  let flakeFile = flakeDir </> "flake.nix"
  writeFile flakeFile $
    unlines
      [ "{",
        "  description = \"Test flake for agent-vm integration tests\";",
        "",
        "  inputs = {",
        "    nixpkgs.url = \"github:NixOS/nixpkgs/nixos-unstable\";",
        "  };",
        "",
        "  outputs = { self, nixpkgs }: {",
        "    devShells = nixpkgs.lib.genAttrs [ \"x86_64-linux\" ] (system:",
        "      let",
        "        pkgs = nixpkgs.legacyPackages.${system};",
        "      in {",
        "        default = pkgs.mkShell {",
        "          buildInputs = with pkgs; [",
        "            hello",
        "            cowsay",
        "          ];",
        "        };",
        "        alternative = pkgs.mkShell {",
        "          buildInputs = with pkgs; [",
        "            figlet",
        "            lolcat",
        "          ];",
        "        };",
        "      }",
        "    );",
        "  };",
        "}"
      ]

  -- Initialize git in the flake directory
  gitInit flakeDir

  -- Add and commit the flake.nix file
  liftIO $ Sh.shelly $ do
    Sh.run_ "git" ["-C", toS flakeDir, "add", "flake.nix"]
    Sh.run_ "git" ["-C", toS flakeDir, "commit", "-m", "Add flake.nix"]

  pure flakeDir

-- | Create a shell fixture (alias for createFlakeFixture for compatibility)
createShellFixture :: FilePath -> IO FilePath
createShellFixture = createFlakeFixture
