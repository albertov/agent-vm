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
      identifier = { name = "plow-log"; version = "0.1.6.0"; };
      license = "MIT";
      copyright = "Plow-Technologies LLC";
      maintainer = "info@plowtech.net";
      author = "Sam Balco";
      homepage = "https://github.com/plow-technologies/plow-log.git#readme";
      url = "";
      synopsis = "Contravariant logging library";
      description = "Contravariant logging library agnostic to the logging backend";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
        ];
        buildable = true;
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/plow-log-0.1.6.0.tar.gz";
      sha256 = "5f374c016cd57e23ce5e8381486c0e693d3c8203a4e5354ddd04c17e054161ed";
    });
  }) // {
    package-description-override = "cabal-version:       >=1.10\r\nname:                plow-log\r\nversion:             0.1.6.0\r\nx-revision: 1\r\nsynopsis:            Contravariant logging library\r\ndescription:         Contravariant logging library agnostic to the logging backend\r\ncategory:            Logging\r\nhomepage:            https://github.com/plow-technologies/plow-log.git#readme\r\nbug-reports:         https://github.com/plow-technologies/plow-log.git/issues\r\ncopyright:           Plow-Technologies LLC\r\nlicense:             MIT\r\nlicense-file:        LICENSE\r\nauthor:              Sam Balco\r\nmaintainer:          info@plowtech.net\r\nbuild-type:          Simple\r\nextra-source-files:  CHANGELOG.md\r\n\r\nsource-repository head\r\n  type: git\r\n  location: https://github.com/plow-technologies/plow-log.git\r\n\r\nlibrary\r\n  exposed-modules:\r\n      Plow.Logging\r\n    , Plow.Logging.Message\r\n    , Plow.Throwing\r\n  other-modules:\r\n      Plow.Logging.EnumerableConstructors\r\n  hs-source-dirs:\r\n      src\r\n  ghc-options: -Wall -Wincomplete-uni-patterns -Wincomplete-record-updates\r\n  build-depends:\r\n      base >= 4.13 && < 4.18\r\n      , aeson >= 2.1.1 && < 2.2\r\n      , text >= 1.2.5 && < 2.1\r\n  default-language: Haskell2010\r\n";
  }