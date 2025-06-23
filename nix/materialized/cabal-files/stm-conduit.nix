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
      specVersion = "1.8";
      identifier = { name = "stm-conduit"; version = "4.0.1"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "cg.wowus.cg@gmail.com";
      author = "Clark Gaebel";
      homepage = "https://github.com/cgaebel/stm-conduit";
      url = "";
      synopsis = "Introduces conduits to channels, and promotes using conduits concurrently.";
      description = "Provides two simple conduit wrappers around STM channels - a source and a sink.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
          (hsPkgs."stm-chans" or (errorHandler.buildDepError "stm-chans"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."cereal-conduit" or (errorHandler.buildDepError "cereal-conduit"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."conduit-extra" or (errorHandler.buildDepError "conduit-extra"))
          (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
          (hsPkgs."exceptions" or (errorHandler.buildDepError "exceptions"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."async" or (errorHandler.buildDepError "async"))
          (hsPkgs."monad-loops" or (errorHandler.buildDepError "monad-loops"))
          (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
        ];
        buildable = true;
      };
      tests = {
        "stm-conduit-doctests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."doctest" or (errorHandler.buildDepError "doctest"))
          ];
          buildable = true;
        };
        "stm-conduit-tests" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."QuickCheck" or (errorHandler.buildDepError "QuickCheck"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."test-framework" or (errorHandler.buildDepError "test-framework"))
            (hsPkgs."test-framework-hunit" or (errorHandler.buildDepError "test-framework-hunit"))
            (hsPkgs."test-framework-quickcheck2" or (errorHandler.buildDepError "test-framework-quickcheck2"))
            (hsPkgs."stm" or (errorHandler.buildDepError "stm"))
            (hsPkgs."stm-conduit" or (errorHandler.buildDepError "stm-conduit"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."stm-chans" or (errorHandler.buildDepError "stm-chans"))
            (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."unliftio" or (errorHandler.buildDepError "unliftio"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/stm-conduit-4.0.1.tar.gz";
      sha256 = "e80e5be72a4564fa45e1e27f91c0984e12d2a736d0ceb9594350d573efee1442";
    });
  }) // {
    package-description-override = "Name:                stm-conduit\r\nVersion:             4.0.1\r\nx-revision: 1\r\nSynopsis:            Introduces conduits to channels, and promotes using conduits concurrently.\r\nDescription:         Provides two simple conduit wrappers around STM channels - a source and a sink.\r\n                     \r\nHomepage:            https://github.com/cgaebel/stm-conduit\r\nLicense:             BSD3\r\nLicense-file:        LICENSE\r\nAuthor:              Clark Gaebel\r\nMaintainer:          cg.wowus.cg@gmail.com\r\nCategory:            Concurrency, Conduit\r\n\r\nBuild-type:          Simple\r\n\r\ntested-with: GHC == 8.0.1, GHC == 8.2.1\r\n\r\nCabal-version:       >=1.8\r\n\r\nLibrary\r\n    exposed-modules:\r\n        Data.Conduit.Async\r\n        Data.Conduit.TMChan\r\n        Data.Conduit.TQueue\r\n        Data.Conduit.Utils\r\n\r\n    other-modules:\r\n        Data.Conduit.Async.Composition\r\n\r\n    build-depends:\r\n        base                == 4.*\r\n      , transformers        >= 0.2 && < 0.7\r\n      , stm                 >= 2.4 && < 2.6\r\n      , stm-chans           >= 2.0 && < 3.1\r\n      , cereal              >= 0.4.0.1\r\n      , cereal-conduit      >= 0.8\r\n      , conduit             >= 1.0 && < 1.4\r\n      , conduit-extra       >= 1.0 && < 1.4\r\n      , directory           >= 1.1\r\n      , exceptions\r\n      , resourcet           >= 0.3 && < 1.4\r\n      , async               >= 2.0.1\r\n      , monad-loops         >= 0.4.2\r\n      , unliftio            >= 0.2.0 && < 0.3.0\r\n\r\n    ghc-options: -Wall -fwarn-tabs -fwarn-unused-imports\r\n\r\ntest-suite stm-conduit-doctests\r\n    type:           exitcode-stdio-1.0\r\n    main-is:        DocTests.hs\r\n    ghc-options:    -threaded\r\n    hs-source-dirs: test/\r\n                    ./\r\n    build-depends:  base\r\n                  , doctest\r\n\r\ntest-suite stm-conduit-tests\r\n    type:           exitcode-stdio-1.0\r\n    main-is:        Test.hs\r\n    hs-source-dirs: test/\r\n\r\n    ghc-options:    -rtsopts=all -threaded\r\n\r\n    build-Depends:\r\n        base       == 4.*\r\n      , QuickCheck >= 2\r\n      , HUnit\r\n      , test-framework\r\n      , test-framework-hunit\r\n      , test-framework-quickcheck2\r\n      , stm\r\n      , stm-conduit\r\n      , conduit\r\n      , transformers\r\n      , stm-chans\r\n      , resourcet\r\n      , directory\r\n      , unliftio\r\n\r\nsource-repository head\r\n    type:     git\r\n    location: git://github.com/cgaebel/stm-conduit.git\r\n";
  }