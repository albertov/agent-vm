inputs: final: prev: {
  python3 = final.lib.recursiveUpdate prev.python3 {
    pkgs.agno = final.callPackage ./pkgs/agno.nix { };
    pkgs.mcp = final.callPackage ./pkgs/mcp.nix { };
  };

  mcp-language-server = final.callPackage ./pkgs/mcp-language-server.nix {
    inherit inputs;
    buildGoModule = final.buildGo124Module;
  };

  codemcp = final.callPackage ./pkgs/codemcp.nix { };

  mcp-proxy = final.callPackage ./pkgs/mcp-proxy.nix {
    inherit inputs;
  };

  mcp-selenium = inputs.mcp-selenium.packages.${final.system}.mcp-selenium-hs;

  # ReScript language server from rescript-lsp input
  rescript-language-server = inputs.rescript-lsp.packages.${final.system}.default;
}
