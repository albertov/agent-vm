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
      identifier = { name = "cereal-conduit"; version = "0.8.0"; };
      license = "BSD-3-Clause";
      copyright = "";
      maintainer = "Michael Snoyman";
      author = "Myles C. Maxfield <myles.maxfield@gmail.com>";
      homepage = "https://github.com/snoyberg/conduit";
      url = "";
      synopsis = "Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits";
      description = "Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits.";
      buildType = "Simple";
    };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
          (hsPkgs."resourcet" or (errorHandler.buildDepError "resourcet"))
          (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
        ];
        buildable = true;
      };
      tests = {
        "test-cereal-conduit" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."conduit" or (errorHandler.buildDepError "conduit"))
            (hsPkgs."cereal" or (errorHandler.buildDepError "cereal"))
            (hsPkgs."cereal-conduit" or (errorHandler.buildDepError "cereal-conduit"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."HUnit" or (errorHandler.buildDepError "HUnit"))
            (hsPkgs."mtl" or (errorHandler.buildDepError "mtl"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
          ];
          buildable = true;
        };
      };
    };
  } // {
    src = pkgs.lib.mkDefault (pkgs.fetchurl {
      url = "http://hackage.haskell.org/package/cereal-conduit-0.8.0.tar.gz";
      sha256 = "d95c4518a9984feacfd811c64be993705bff74c1f2daa00b4687bbb79f3a39eb";
    });
  }) // {
    package-description-override = "name:            cereal-conduit\r\nversion:         0.8.0\r\nx-revision: 2\r\nlicense:         BSD3\r\nlicense-file:    LICENSE\r\nauthor:          Myles C. Maxfield <myles.maxfield@gmail.com>\r\nmaintainer:      Michael Snoyman\r\nsynopsis:        Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits\r\ndescription:\r\n    Turn Data.Serialize Gets and Puts into Sources, Sinks, and Conduits.\r\ncategory:        Conduit\r\nstability:       Experimental\r\ncabal-version:   >= 1.8\r\nbuild-type:      Simple\r\nhomepage:        https://github.com/snoyberg/conduit\r\nbug-reports:     https://github.com/snoyberg/conduit/issues\r\nextra-source-files: README.md ChangeLog.md\r\n\r\nlibrary\r\n    build-depends: base         >= 4.9     && < 5\r\n                 , conduit      >= 1.3     && < 1.4\r\n                 , resourcet    >= 1.2     && < 1.4\r\n                 , cereal       >= 0.4.0.0 && < 0.6\r\n                 , bytestring\r\n                 , transformers >= 0.2.0.0\r\n    exposed-modules: Data.Conduit.Cereal\r\n                   , Data.Conduit.Cereal.Internal\r\n    ghc-options:     -Wall\r\n\r\nTest-Suite test-cereal-conduit\r\n    type: exitcode-stdio-1.0\r\n    main-is: Main.hs\r\n    hs-source-dirs: Test\r\n    build-depends: base\r\n                 , conduit\r\n                 , cereal\r\n                 , cereal-conduit\r\n                 , bytestring\r\n                 --, test-framework-hunit\r\n                 , HUnit\r\n                 , mtl\r\n                 , transformers\r\n\r\nsource-repository head\r\n  type:     git\r\n  location: git://github.com/snoyberg/conduit.git\r\n";
  }