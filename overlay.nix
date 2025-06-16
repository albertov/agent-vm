inputs: final: prev:
{

  mkMCPDevServers = { name ? "start-agent", shell, pkgs ? final }:
    let
      hookFile = pkgs.writeText "shellHook.source" (shell.shellHook or "");
      shellInputs =
        shell.buildInputs or [ ]
        ++ shell.nativeBuildInputs or [ ];

    in
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs = with pkgs;
        [
          git
          nix
          codemcp
          mcp-proxy
          mcp-language-server
          rescript-language-server
          inputs.mcp-selenium.packages.${system}.default
          inputs.mcp-nixos.packages.${system}.default
        ] ++ shellInputs;
      excludeShellChecks = [
        "SC1091" # So we can 'source' the shellHook
      ];
      text = ''
        source ${hookFile}
        exec ${./start.sh} "$@"
      '';
    };


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

  mcp-selenium =
    inputs.mcp-selenium.packages.${final.system}.mcp-selenium-hs;

  # VM management tool for agent isolation
  agent-vm = final.python3.pkgs.buildPythonApplication {
    pname = "agent-vm";
    version = "1.0.0";
    src = ./agent-vm.py;
    format = "other";
    dontUnpack = true;

    # Only Python stdlib - no dependencies
    propagatedBuildInputs = [ ];

    installPhase = ''
      mkdir -p $out/bin
      cp $src $out/bin/agent-vm
      chmod +x $out/bin/agent-vm
    '';

    # Type checking during build
    checkPhase = ''
      ${final.python3}/bin/python -m py_compile $src
    '';

    meta = {
      description = "VM control tool for managing development VMs";
      longDescription = ''
        A Python-based VM management tool that provides comprehensive lifecycle
        management for development VMs including creation, startup, shutdown,
        and workspace management for agent development environments.
      '';
      maintainers = [ ];
      platforms = final.lib.platforms.unix;
    };
  };
}
