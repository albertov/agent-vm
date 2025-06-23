{ pkgs, ... }:
rec {
  name = "agent-vm";
  compiler-nix-name = "ghc910"; # Version of GHC to use
  checkMaterialization = false;
  materialized = ./materialized;

  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.ormolu = "latest";
  shell.tools.ghcid = "latest";
  shell.tools.haskell-language-server = "latest";
  shell.withHoogle = false;
  # Non-Haskell tools to include in the development shell
  shell.nativeBuildInputs = with pkgs.buildPackages; [
    hoogle # This one has our local packages!
    gh # GitHub CLI for PR automation
  ];
  shell.shellHook = ''
    cabal update
  '';

}
