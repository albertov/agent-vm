{
  pkgs = hackage:
    {
      packages = {
        conduit.revision = import ./cabal-files/conduit.nix;
        filepath.revision = hackage.filepath."1.5.4.0".revisions.default;
        attoparsec.revision = import ./cabal-files/attoparsec.nix;
        attoparsec.flags.developer = false;
        text-iso8601.revision = import ./cabal-files/text-iso8601.nix;
        generically.revision = import ./cabal-files/generically.nix;
        generic-lens-core.revision = import ./cabal-files/generic-lens-core.nix;
        protolude.revision = import ./cabal-files/protolude.nix;
        unordered-containers.revision = import ./cabal-files/unordered-containers.nix;
        unordered-containers.flags.debug = false;
        OneTuple.revision = import ./cabal-files/OneTuple.nix;
        tasty.revision = import ./cabal-files/tasty.nix;
        tasty.flags.unix = true;
        data-fix.revision = import ./cabal-files/data-fix.nix;
        HUnit.revision = import ./cabal-files/HUnit.nix;
        haskell-lexer.revision = import ./cabal-files/haskell-lexer.nix;
        uuid-types.revision = import ./cabal-files/uuid-types.nix;
        quickcheck-io.revision = import ./cabal-files/quickcheck-io.nix;
        vector-stream.revision = import ./cabal-files/vector-stream.nix;
        comonad.revision = import ./cabal-files/comonad.nix;
        comonad.flags.containers = true;
        comonad.flags.indexed-traversable = true;
        comonad.flags.distributive = true;
        ghc-bignum.revision = hackage.ghc-bignum."1.3".revisions.default;
        stm.revision = hackage.stm."2.5.3.1".revisions.default;
        integer-conversion.revision = import ./cabal-files/integer-conversion.nix;
        typed-process.revision = import ./cabal-files/typed-process.nix;
        transformers.revision = hackage.transformers."0.6.1.1".revisions.default;
        microlens.revision = import ./cabal-files/microlens.nix;
        distributive.revision = import ./cabal-files/distributive.nix;
        distributive.flags.semigroups = true;
        distributive.flags.tagged = true;
        deepseq.revision = hackage.deepseq."1.5.0.0".revisions.default;
        optparse-applicative.revision = import ./cabal-files/optparse-applicative.nix;
        optparse-applicative.flags.process = true;
        streaming-commons.revision = import ./cabal-files/streaming-commons.nix;
        streaming-commons.flags.use-bytestring-builder = false;
        split.revision = import ./cabal-files/split.nix;
        directory.revision = hackage.directory."1.3.8.5".revisions.default;
        stm-chans.revision = import ./cabal-files/stm-chans.nix;
        parsec.revision = hackage.parsec."3.1.18.0".revisions.default;
        cereal-conduit.revision = import ./cabal-files/cereal-conduit.nix;
        strict.revision = import ./cabal-files/strict.nix;
        generic-lens.revision = import ./cabal-files/generic-lens.nix;
        th-abstraction.revision = import ./cabal-files/th-abstraction.nix;
        unbounded-delays.revision = import ./cabal-files/unbounded-delays.nix;
        profunctors.revision = import ./cabal-files/profunctors.nix;
        microlens-mtl.revision = import ./cabal-files/microlens-mtl.nix;
        mtl.revision = hackage.mtl."2.3.1".revisions.default;
        ansi-terminal-types.revision = import ./cabal-files/ansi-terminal-types.nix;
        process.revision = hackage.process."1.6.25.0".revisions.default;
        base.revision = hackage.base."4.20.1.0".revisions.default;
        call-stack.revision = import ./cabal-files/call-stack.nix;
        base-orphans.revision = import ./cabal-files/base-orphans.nix;
        indexed-profunctors.revision = import ./cabal-files/indexed-profunctors.nix;
        QuickCheck.revision = import ./cabal-files/QuickCheck.nix;
        QuickCheck.flags.old-random = false;
        QuickCheck.flags.templatehaskell = true;
        hspec.revision = import ./cabal-files/hspec.nix;
        network-uri.revision = import ./cabal-files/network-uri.nix;
        indexed-traversable-instances.revision = import ./cabal-files/indexed-traversable-instances.nix;
        bitvec.revision = import ./cabal-files/bitvec.nix;
        bitvec.flags.simd = true;
        these.revision = import ./cabal-files/these.nix;
        random.revision = import ./cabal-files/random.nix;
        async.revision = import ./cabal-files/async.nix;
        async.flags.bench = false;
        mtl-compat.revision = import ./cabal-files/mtl-compat.nix;
        mtl-compat.flags.two-point-one = false;
        mtl-compat.flags.two-point-two = false;
        bifunctors.revision = import ./cabal-files/bifunctors.nix;
        bifunctors.flags.tagged = true;
        text.revision = hackage.text."2.1.2".revisions.default;
        safe-exceptions.revision = import ./cabal-files/safe-exceptions.nix;
        time.revision = hackage.time."1.12.2".revisions.default;
        array.revision = hackage.array."0.5.8.0".revisions.default;
        tf-random.revision = import ./cabal-files/tf-random.nix;
        vector-algorithms.revision = import ./cabal-files/vector-algorithms.nix;
        vector-algorithms.flags.bench = true;
        vector-algorithms.flags.internalchecks = false;
        vector-algorithms.flags.boundschecks = true;
        vector-algorithms.flags.unsafechecks = false;
        vector-algorithms.flags.llvm = false;
        semialign.revision = import ./cabal-files/semialign.nix;
        semialign.flags.semigroupoids = true;
        hspec-discover.revision = import ./cabal-files/hspec-discover.nix;
        text-short.revision = import ./cabal-files/text-short.nix;
        text-short.flags.asserts = false;
        semigroupoids.revision = import ./cabal-files/semigroupoids.nix;
        semigroupoids.flags.unordered-containers = true;
        semigroupoids.flags.containers = true;
        semigroupoids.flags.comonad = true;
        semigroupoids.flags.tagged = true;
        semigroupoids.flags.contravariant = true;
        semigroupoids.flags.distributive = true;
        contravariant.revision = import ./cabal-files/contravariant.nix;
        contravariant.flags.semigroups = true;
        contravariant.flags.statevar = true;
        contravariant.flags.tagged = true;
        StateVar.revision = import ./cabal-files/StateVar.nix;
        network.revision = import ./cabal-files/network.nix;
        network.flags.devel = false;
        hspec-core.revision = import ./cabal-files/hspec-core.nix;
        unliftio-core.revision = import ./cabal-files/unliftio-core.nix;
        resourcet.revision = import ./cabal-files/resourcet.nix;
        mono-traversable.revision = import ./cabal-files/mono-traversable.nix;
        indexed-traversable.revision = import ./cabal-files/indexed-traversable.nix;
        hashable.revision = import ./cabal-files/hashable.nix;
        hashable.flags.integer-gmp = true;
        hashable.flags.random-initial-seed = false;
        hashable.flags.arch-native = false;
        character-ps.revision = import ./cabal-files/character-ps.nix;
        th-compat.revision = import ./cabal-files/th-compat.nix;
        ghc-internal.revision = hackage.ghc-internal."9.1002.0".revisions.default;
        conduit-extra.revision = import ./cabal-files/conduit-extra.nix;
        binary.revision = hackage.binary."0.8.9.3".revisions.default;
        monad-loops.revision = import ./cabal-files/monad-loops.nix;
        monad-loops.flags.base4 = true;
        plow-log.revision = import ./cabal-files/plow-log.nix;
        template-haskell.revision = hackage.template-haskell."2.22.0.0".revisions.default;
        unix.revision = hackage.unix."2.8.6.0".revisions.default;
        primitive.revision = import ./cabal-files/primitive.nix;
        aeson.revision = import ./cabal-files/aeson.nix;
        aeson.flags.ordered-keymap = true;
        ansi-terminal.revision = import ./cabal-files/ansi-terminal.nix;
        ansi-terminal.flags.example = false;
        hsc2hs.revision = import ./cabal-files/hsc2hs.nix;
        hsc2hs.flags.in-ghc-tree = false;
        colour.revision = import ./cabal-files/colour.nix;
        exceptions.revision = hackage.exceptions."0.10.9".revisions.default;
        bytestring.revision = hackage.bytestring."0.12.2.0".revisions.default;
        witherable.revision = import ./cabal-files/witherable.nix;
        stm-conduit.revision = import ./cabal-files/stm-conduit.nix;
        unliftio.revision = import ./cabal-files/unliftio.nix;
        integer-logarithms.revision = import ./cabal-files/integer-logarithms.nix;
        integer-logarithms.flags.integer-gmp = true;
        integer-logarithms.flags.check-bounds = false;
        time-compat.revision = import ./cabal-files/time-compat.nix;
        zlib-clib.revision = import ./cabal-files/zlib-clib.nix;
        tagged.revision = import ./cabal-files/tagged.nix;
        tagged.flags.transformers = true;
        tagged.flags.deepseq = true;
        ghc-boot-th.revision = hackage.ghc-boot-th."9.10.2".revisions.default;
        os-string.revision = hackage.os-string."2.0.4".revisions.default;
        transformers-compat.revision = import ./cabal-files/transformers-compat.nix;
        transformers-compat.flags.three = false;
        transformers-compat.flags.four = false;
        transformers-compat.flags.five-three = true;
        transformers-compat.flags.mtl = true;
        transformers-compat.flags.generic-deriving = true;
        transformers-compat.flags.two = false;
        transformers-compat.flags.five = false;
        temporary.revision = import ./cabal-files/temporary.nix;
        prettyprinter.revision = import ./cabal-files/prettyprinter.nix;
        prettyprinter.flags.buildreadme = false;
        prettyprinter.flags.text = true;
        assoc.revision = import ./cabal-files/assoc.nix;
        assoc.flags.tagged = false;
        ghc-prim.revision = hackage.ghc-prim."0.12.0".revisions.default;
        hspec-expectations.revision = import ./cabal-files/hspec-expectations.nix;
        pretty.revision = hackage.pretty."1.1.3.6".revisions.default;
        plow-log-async.revision = import ./cabal-files/plow-log-async.nix;
        zlib.revision = import ./cabal-files/zlib.nix;
        zlib.flags.bundled-c-zlib = true;
        zlib.flags.non-blocking-ffi = true;
        zlib.flags.pkg-config = true;
        splitmix.revision = import ./cabal-files/splitmix.nix;
        splitmix.flags.optimised-mixer = false;
        cereal.revision = import ./cabal-files/cereal.nix;
        cereal.flags.bytestring-builder = false;
        containers.revision = hackage.containers."0.7".revisions.default;
        prettyprinter-ansi-terminal.revision = import ./cabal-files/prettyprinter-ansi-terminal.nix;
        scientific.revision = import ./cabal-files/scientific.nix;
        scientific.flags.integer-simple = false;
        vector.revision = import ./cabal-files/vector.nix;
        vector.flags.internalchecks = false;
        vector.flags.boundschecks = true;
        vector.flags.wall = false;
        vector.flags.unsafechecks = false;
        dlist.revision = import ./cabal-files/dlist.nix;
        dlist.flags.werror = false;
      };
      compiler = {
        version = "9.10.2";
        nix-name = "ghc9102";
        packages = {
          "ghc-boot-th" = "9.10.2";
          "binary" = "0.8.9.3";
          "pretty" = "1.1.3.6";
          "array" = "0.5.8.0";
          "time" = "1.12.2";
          "ghc-prim" = "0.12.0";
          "bytestring" = "0.12.2.0";
          "process" = "1.6.25.0";
          "mtl" = "2.3.1";
          "text" = "2.1.2";
          "template-haskell" = "2.22.0.0";
          "parsec" = "3.1.18.0";
          "ghc-bignum" = "1.3";
          "stm" = "2.5.3.1";
          "filepath" = "1.5.4.0";
          "os-string" = "2.0.4";
          "unix" = "2.8.6.0";
          "exceptions" = "0.10.9";
          "deepseq" = "1.5.0.0";
          "transformers" = "0.6.1.1";
          "containers" = "0.7";
          "ghc-internal" = "9.1002.0";
          "base" = "4.20.1.0";
          "directory" = "1.3.8.5";
        };
      };
    };
  extras = hackage:
    { packages = { agent-vm = ./.plan.nix/agent-vm.nix; }; };
  modules = [
    {
      preExistingPkgs = [
        "filepath"
        "ghc-bignum"
        "stm"
        "transformers"
        "deepseq"
        "directory"
        "parsec"
        "mtl"
        "process"
        "base"
        "text"
        "time"
        "array"
        "ghc-internal"
        "binary"
        "template-haskell"
        "unix"
        "exceptions"
        "bytestring"
        "ghc-boot-th"
        "os-string"
        "ghc-prim"
        "pretty"
        "containers"
      ];
    }
    ({ lib, ... }:
      { packages = { "agent-vm" = { flags = {}; }; }; })
    ({ lib, ... }:
      {
        packages = {
          "data-fix".components.library.planned = lib.mkOverride 900 true;
          "tf-random".components.library.planned = lib.mkOverride 900 true;
          "integer-conversion".components.library.planned = lib.mkOverride 900 true;
          "monad-loops".components.library.planned = lib.mkOverride 900 true;
          "quickcheck-io".components.library.planned = lib.mkOverride 900 true;
          "generic-lens".components.library.planned = lib.mkOverride 900 true;
          "generic-lens-core".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.sublibs."attoparsec-internal".planned = lib.mkOverride 900 true;
          "resourcet".components.library.planned = lib.mkOverride 900 true;
          "mono-traversable".components.library.planned = lib.mkOverride 900 true;
          "conduit".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.library.planned = lib.mkOverride 900 true;
          "hashable".components.library.planned = lib.mkOverride 900 true;
          "template-haskell".components.library.planned = lib.mkOverride 900 true;
          "text-short".components.library.planned = lib.mkOverride 900 true;
          "network-uri".components.library.planned = lib.mkOverride 900 true;
          "unordered-containers".components.library.planned = lib.mkOverride 900 true;
          "these".components.library.planned = lib.mkOverride 900 true;
          "QuickCheck".components.library.planned = lib.mkOverride 900 true;
          "cereal-conduit".components.library.planned = lib.mkOverride 900 true;
          "distributive".components.library.planned = lib.mkOverride 900 true;
          "transformers".components.library.planned = lib.mkOverride 900 true;
          "hspec-core".components.library.planned = lib.mkOverride 900 true;
          "unliftio-core".components.library.planned = lib.mkOverride 900 true;
          "profunctors".components.library.planned = lib.mkOverride 900 true;
          "scientific".components.library.planned = lib.mkOverride 900 true;
          "array".components.library.planned = lib.mkOverride 900 true;
          "unix".components.library.planned = lib.mkOverride 900 true;
          "base-orphans".components.library.planned = lib.mkOverride 900 true;
          "call-stack".components.library.planned = lib.mkOverride 900 true;
          "exceptions".components.library.planned = lib.mkOverride 900 true;
          "unliftio".components.library.planned = lib.mkOverride 900 true;
          "plow-log".components.library.planned = lib.mkOverride 900 true;
          "directory".components.library.planned = lib.mkOverride 900 true;
          "HUnit".components.library.planned = lib.mkOverride 900 true;
          "semigroupoids".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable-instances".components.library.planned = lib.mkOverride 900 true;
          "stm-conduit".components.library.planned = lib.mkOverride 900 true;
          "indexed-profunctors".components.library.planned = lib.mkOverride 900 true;
          "containers".components.library.planned = lib.mkOverride 900 true;
          "parsec".components.library.planned = lib.mkOverride 900 true;
          "semialign".components.library.planned = lib.mkOverride 900 true;
          "random".components.library.planned = lib.mkOverride 900 true;
          "dlist".components.library.planned = lib.mkOverride 900 true;
          "vector-algorithms".components.library.planned = lib.mkOverride 900 true;
          "zlib-clib".components.library.planned = lib.mkOverride 900 true;
          "aeson".components.library.planned = lib.mkOverride 900 true;
          "unbounded-delays".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter".components.library.planned = lib.mkOverride 900 true;
          "hspec-discover".components.exes."hspec-discover".planned = lib.mkOverride 900 true;
          "transformers-compat".components.library.planned = lib.mkOverride 900 true;
          "time-compat".components.library.planned = lib.mkOverride 900 true;
          "hspec".components.library.planned = lib.mkOverride 900 true;
          "conduit-extra".components.library.planned = lib.mkOverride 900 true;
          "typed-process".components.library.planned = lib.mkOverride 900 true;
          "safe-exceptions".components.library.planned = lib.mkOverride 900 true;
          "StateVar".components.library.planned = lib.mkOverride 900 true;
          "stm".components.library.planned = lib.mkOverride 900 true;
          "strict".components.library.planned = lib.mkOverride 900 true;
          "cereal".components.library.planned = lib.mkOverride 900 true;
          "network".components.library.planned = lib.mkOverride 900 true;
          "comonad".components.library.planned = lib.mkOverride 900 true;
          "bytestring".components.library.planned = lib.mkOverride 900 true;
          "bitvec".components.library.planned = lib.mkOverride 900 true;
          "streaming-commons".components.library.planned = lib.mkOverride 900 true;
          "attoparsec".components.library.planned = lib.mkOverride 900 true;
          "colour".components.library.planned = lib.mkOverride 900 true;
          "integer-logarithms".components.library.planned = lib.mkOverride 900 true;
          "deepseq".components.library.planned = lib.mkOverride 900 true;
          "agent-vm".components.tests."spec".planned = lib.mkOverride 900 true;
          "agent-vm".components.exes."agent-vm".planned = lib.mkOverride 900 true;
          "optparse-applicative".components.library.planned = lib.mkOverride 900 true;
          "plow-log-async".components.library.planned = lib.mkOverride 900 true;
          "agent-vm".components.library.planned = lib.mkOverride 900 true;
          "tagged".components.library.planned = lib.mkOverride 900 true;
          "zlib".components.library.planned = lib.mkOverride 900 true;
          "filepath".components.library.planned = lib.mkOverride 900 true;
          "assoc".components.library.planned = lib.mkOverride 900 true;
          "OneTuple".components.library.planned = lib.mkOverride 900 true;
          "witherable".components.library.planned = lib.mkOverride 900 true;
          "ghc-internal".components.library.planned = lib.mkOverride 900 true;
          "time".components.library.planned = lib.mkOverride 900 true;
          "primitive".components.library.planned = lib.mkOverride 900 true;
          "ghc-bignum".components.library.planned = lib.mkOverride 900 true;
          "pretty".components.library.planned = lib.mkOverride 900 true;
          "th-compat".components.library.planned = lib.mkOverride 900 true;
          "haskell-lexer".components.library.planned = lib.mkOverride 900 true;
          "microlens-mtl".components.library.planned = lib.mkOverride 900 true;
          "character-ps".components.library.planned = lib.mkOverride 900 true;
          "os-string".components.library.planned = lib.mkOverride 900 true;
          "agent-vm".components.exes."agent-vm-test".planned = lib.mkOverride 900 true;
          "split".components.library.planned = lib.mkOverride 900 true;
          "prettyprinter-ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "th-abstraction".components.library.planned = lib.mkOverride 900 true;
          "stm-chans".components.library.planned = lib.mkOverride 900 true;
          "protolude".components.library.planned = lib.mkOverride 900 true;
          "mtl-compat".components.library.planned = lib.mkOverride 900 true;
          "mtl".components.library.planned = lib.mkOverride 900 true;
          "bifunctors".components.library.planned = lib.mkOverride 900 true;
          "uuid-types".components.library.planned = lib.mkOverride 900 true;
          "vector".components.sublibs."benchmarks-O2".planned = lib.mkOverride 900 true;
          "text-iso8601".components.library.planned = lib.mkOverride 900 true;
          "binary".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal-types".components.library.planned = lib.mkOverride 900 true;
          "ghc-boot-th".components.library.planned = lib.mkOverride 900 true;
          "ansi-terminal".components.library.planned = lib.mkOverride 900 true;
          "hspec-expectations".components.library.planned = lib.mkOverride 900 true;
          "microlens".components.library.planned = lib.mkOverride 900 true;
          "hsc2hs".components.exes."hsc2hs".planned = lib.mkOverride 900 true;
          "vector-stream".components.library.planned = lib.mkOverride 900 true;
          "temporary".components.library.planned = lib.mkOverride 900 true;
          "base".components.library.planned = lib.mkOverride 900 true;
          "process".components.library.planned = lib.mkOverride 900 true;
          "vector".components.library.planned = lib.mkOverride 900 true;
          "contravariant".components.library.planned = lib.mkOverride 900 true;
          "tasty".components.library.planned = lib.mkOverride 900 true;
          "text".components.library.planned = lib.mkOverride 900 true;
          "indexed-traversable".components.library.planned = lib.mkOverride 900 true;
          "generically".components.library.planned = lib.mkOverride 900 true;
          "splitmix".components.library.planned = lib.mkOverride 900 true;
          "ghc-prim".components.library.planned = lib.mkOverride 900 true;
          "async".components.library.planned = lib.mkOverride 900 true;
        };
      })
  ];
}