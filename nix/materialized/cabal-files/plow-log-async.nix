{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  ({
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "plow-log-async"; version = "0.1.4.0"; };
      license = "MIT";
      copyright = "Plow-Technologies LLC";
      maintainer = "info@plowtech.net";
      author = "Alberto Valverde";
      homepage = "https://github.com/plow-technologies/plow-log-async.git#readme";
      url = "";
      synopsis = "Async IO tracer for plow-log";
      description = "Async logging backend for plow-log ";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."plow-log" or (errorHandler.buildDepError "plow-log"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."stm-conduit" or (errorHandler.buildDepError "stm-conduit"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          (hsPkgs."time" or (errorHandler.buildDepError "time"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/plow-log-async-0.1.4.0.tar.gz";
      sha256 = "55dc50292396b0d329da6b4958ee9c63830ef34d339494a8f2834db7033c8f93";
    });
  }) // {
    package-description-override = "cabal-version:       >=1.10\nname:                plow-log-async\nversion:             0.1.4.0\nsynopsis:            Async IO tracer for plow-log\ndescription:         Async logging backend for plow-log \ncategory:            Logging\nhomepage:            https://github.com/plow-technologies/plow-log-async.git#readme\nbug-reports:         https://github.com/plow-technologies/plow-log-async.git/issues\ncopyright:           Plow-Technologies LLC\nlicense:             MIT\nlicense-file:        LICENSE\nauthor:              Alberto Valverde\nmaintainer:          info@plowtech.net\nbuild-type:          Simple\nextra-source-files:  CHANGELOG.md\n\nsource-repository head\n  type: git\n  location: https://github.com/plow-technologies/plow-log-async.git\n\nlibrary\n  exposed-modules:\n    Plow.Logging.Async\n  hs-source-dirs:\n      src\n  ghc-options: -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates\n  build-depends:\n      base >= 4.13 && < 4.18\n    , plow-log >= 0.1.6 && < 0.2\n    , conduit >= 1.3.4 && < 1.4\n    , stm-conduit >= 4.0.1 && < 4.1\n    , text >= 2.0.1 && < 2.1\n    , unliftio >= 0.2.22 && < 0.3\n    , time >= 1.9.3 && < 1.12\n  default-language: Haskell2010\n";
  }