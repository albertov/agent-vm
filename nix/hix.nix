{ pkgs, ... }:
rec {
  name = "agent-vm";
  compiler-nix-name = "ghc967"; # Version of GHC to use
  index-state = "2025-06-16T20:56:04Z";
  checkMaterialization = false;
  materialized = ./materialized;

  shell.withHoogle = false;
  # Tools to include in the development shell
  shell.tools.cabal = "latest";
  shell.tools.hlint = "latest";
  shell.tools.ormolu = "latest";
  shell.tools.ghcid = "latest";
  shell.tools.haskell-language-server = "latest";
  # Non-Haskell tools to include in the development shell
  shell.nativeBuildInputs = with pkgs.buildPackages; [
    hoogle # This one has our local packages!
    gh # GitHub CLI for PR automation
  ];
}
