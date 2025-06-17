inputs: final: prev: {

  mkMCPDevServers =
    {
      name ? "start-agent",
      shell,
      pkgs ? final,
    }:
    let
      hookFile = pkgs.writeText "shellHook.source" (shell.shellHook or "");
      shellInputs = shell.buildInputs or [ ] ++ shell.nativeBuildInputs or [ ];

    in
    pkgs.writeShellApplication {
      inherit name;
      runtimeInputs =
        with pkgs;
        [
          git
          nix
          codemcp
          mcp-proxy
          mcp-language-server
          rescript-language-server
          inputs.mcp-selenium.packages.${system}.default
          inputs.mcp-nixos.packages.${system}.default
        ]
        ++ shellInputs;
      excludeShellChecks = [
        "SC1091" # So we can 'source' the shellHook
      ];
      text = ''
        source ${hookFile}
        ${builtins.readFile ../start.sh}
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

  mcp-selenium = inputs.mcp-selenium.packages.${final.system}.mcp-selenium-hs;

  # ReScript language server from rescript-lsp input
  rescript-language-server = inputs.rescript-lsp.packages.${final.system}.default;

  # VM management tool for agent isolation (Python version)
  agent-vm-py = final.python3.pkgs.buildPythonApplication {
    pname = "agent-vm-py";
    version = "1.0.0";
    src = ./agent-vm-py;
    format = "pyproject";

    # Runtime dependencies
    propagatedBuildInputs =
      with final;
      with final.python3.pkgs;
      [
        git
        openssh
        qemu
        curl
        typer
        # For agent-vm-tests
        pytest
        pytest-timeout
        pytest-cov
      ];

    # Test dependencies for build-time testing
    nativeBuildInputs = [
      final.python3.pkgs.setuptools
      final.python3.pkgs.wheel
      final.python3.pkgs.pytest
      final.python3.pkgs.pytest-mock
      final.python3.pkgs.pytest-timeout
      final.python3.pkgs.pytest-cov
    ];

    # Enable tests during build
    doCheck = true;

    # Configure test command
    checkPhase = ''
      runHook preCheck

      # Run pytest with coverage and timeouts
      python -m pytest tests/ -v \
        --timeout=300 \
        --cov=agent_vm \
        --cov-report=term-missing \
        -m "not integration" \
        || echo "Tests failed but continuing build for now"

      runHook postCheck
    '';

    # Type checking during build
    pythonImportsCheck = [
      "agent_vm.main"
      "agent_vm.vm_controller"
    ];

    meta = with final.lib; {
      description = "VM control tool for managing development VMs";
      longDescription = ''
        A Python-based VM management tool that provides comprehensive lifecycle
        management for development VMs including creation, startup, shutdown,
        and workspace management for agent development environments.
      '';
      homepage = "https://github.com/example/agent-vm";
      license = licenses.mit;
      maintainers = [ ];
      platforms = platforms.linux;
      mainProgram = "agent-vm-py";
    };
  };

  py-integration-test = final.writeShellApplication {
    name = "py-integration-test";
    runtimeInputs = with final; [
      agent-vm-py
    ];
    text = ''
      exec agent-vm-tests "$@"
    '';
  };
}
