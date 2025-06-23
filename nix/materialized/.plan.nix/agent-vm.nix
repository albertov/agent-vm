{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "3.0";
      identifier = { name = "agent-vm"; version = "0.1.0.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "alberto@toscat.net";
      author = "Alberto Valverde";
      homepage = "";
      url = "";
      synopsis = "Type-safe VM lifecycle management with Nix integration";
      description = "QEMU VM management system with compile-time state guarantees";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [];
      dataDir = ".";
      dataFiles = [
        "test/fixtures/ProcessSpec/echo_both.sh"
        "test/fixtures/ProcessSpec/exit_failure.sh"
        "test/fixtures/ProcessSpec/rapid_output.sh"
        "test/fixtures/ProcessSpec/echo_stderr.sh"
        "test/fixtures/ProcessSpec/multiline_output.sh"
        "test/fixtures/ProcessSpec/unexpected_termination.sh"
        "test/fixtures/ProcessSpec/echo_stdout.sh"
        "test/fixtures/ProcessSpec/no_output.sh"
        "test/fixtures/ProcessSpec/zombie_process.sh"
      ];
      extraSrcFiles = [ "test/fixtures/ProcessSpec/*.sh" ];
      extraTmpFiles = [];
      extraDocFiles = [];
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."unbounded-delays" or (errorHandler.buildDepError "unbounded-delays"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."ansi-terminal" or (errorHandler.buildDepError "ansi-terminal"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
          (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
          (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
          (hsPkgs."plow-log" or (errorHandler.buildDepError "plow-log"))
          (hsPkgs."plow-log-async" or (errorHandler.buildDepError "plow-log-async"))
          (hsPkgs."network" or (errorHandler.buildDepError "network"))
          (hsPkgs."microlens" or (errorHandler.buildDepError "microlens"))
          (hsPkgs."microlens-mtl" or (errorHandler.buildDepError "microlens-mtl"))
          (hsPkgs."generic-lens" or (errorHandler.buildDepError "generic-lens"))
          (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
          (hsPkgs."unix" or (errorHandler.buildDepError "unix"))
        ];
        buildable = true;
        modules = [
          "AgentVM"
          "AgentVM/Types"
          "AgentVM/State"
          "AgentVM/Process"
          "AgentVM/SSH"
          "AgentVM/Nix"
          "AgentVM/Log"
          "AgentVM/Config"
          "AgentVM/Env"
          "AgentVM/Class"
          "AgentVM/Monad"
        ];
        hsSourceDirs = [ "src" ];
      };
      exes = {
        "agent-vm" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."agent-vm" or (errorHandler.buildDepError "agent-vm"))
            (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
            (hsPkgs."plow-log-async" or (errorHandler.buildDepError "plow-log-async"))
            (hsPkgs."plow-log" or (errorHandler.buildDepError "plow-log"))
          ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
        };
        "agent-vm-test" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."agent-vm" or (errorHandler.buildDepError "agent-vm"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."unbounded-delays" or (errorHandler.buildDepError "unbounded-delays"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
          ];
          buildable = true;
          hsSourceDirs = [ "integration-test" ];
          mainPath = [ "Main.hs" ];
        };
      };
      tests = {
        "spec" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."protolude" or (errorHandler.buildDepError "protolude"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
            (hsPkgs."agent-vm" or (errorHandler.buildDepError "agent-vm"))
            (hsPkgs."hspec" or (errorHandler.buildDepError "hspec"))
            (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
            (hsPkgs."typed-process" or (errorHandler.buildDepError "typed-process"))
            (hsPkgs."temporary" or (errorHandler.buildDepError "temporary"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
            (hsPkgs."plow-log" or (errorHandler.buildDepError "plow-log"))
            (hsPkgs."plow-log-async" or (errorHandler.buildDepError "plow-log-async"))
            (hsPkgs."network" or (errorHandler.buildDepError "network"))
            (hsPkgs."unbounded-delays" or (errorHandler.buildDepError "unbounded-delays"))
            (hsPkgs."filepath" or (errorHandler.buildDepError "filepath"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          ];
          build-tools = [
            (hsPkgs.pkgsBuildBuild.hspec-discover.components.exes.hspec-discover or (pkgs.pkgsBuildBuild.hspec-discover or (errorHandler.buildToolDepError "hspec-discover:hspec-discover")))
          ];
          buildable = true;
          modules = [
            "AgentVM/StateSpec"
            "AgentVM/ProcessSpec"
            "AgentVM/NixSpec"
            "Paths_agent_vm"
          ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
        };
      };
    };
  } // rec { src = pkgs.lib.mkDefault .././.; }