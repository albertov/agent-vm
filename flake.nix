{
  description = "Agent VM - Isolated MCP agent development environment using QEMU VMs";

  nixConfig = {
    bash-prompt = "\\[\\e[0;37m\\](\\[\\e[0m\\]nix) \\[\\e[0;1;94m\\]agent-vm \\[\\e[0m\\]\\w \\[\\e[0;1m\\]λ \\[\\e[0m\\]";
    extra-substituters = [
      "https://mcp-selenium-haskell.cachix.org"
    ];
    extra-trusted-public-keys = [
      "mcp-selenium-haskell.cachix.org-1:C+mSRd39ugTt5+QWvgPRVmGYnHBMFu0+8HW0oW8uA+Y="
    ];
  };

  inputs = {
    # Core inputs
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    treefmt-nix.url = "github:numtide/treefmt-nix";

    # ReScript language server
    rescript-lsp.url = "github:frectonz/rescript-lsp-nix";
    rescript-lsp.inputs.nixpkgs.follows = "nixpkgs";

    # MCP packages
    mcp-proxy = {
      url = "github:sparfenyuk/mcp-proxy/v0.8.0";
      flake = false;
    };

    mcp-language-server = {
      url = "github:isaacphi/mcp-language-server";
      flake = false;
    };

    mcp-selenium.url = "github:albertov/mcp-selenium-haskell";

    mcp-nixos.url = "github:utensils/mcp-nixos";
    mcp-nixos.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = [
            # Agent overlay
            (import ./overlay.nix inputs)
          ];
        };

        # Default shell for development
        defaultShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            git
            nix
            openssh
            qemu
            curl
          ];
        };

      in
      {
        # Legacy packages for compatibility
        legacyPackages = pkgs;

        # Main packages
        packages = rec {
          # Agent VM management tool
          agent-vm = pkgs.agent-vm;

          # MCP packages
          codemcp = pkgs.codemcp;
          mcp-proxy = pkgs.mcp-proxy;
          mcp-language-server = pkgs.mcp-language-server;
          rescript-language-server = inputs.rescript-lsp.packages.${system}.default;

          # Python packages
          python3Packages-agno = pkgs.python3.pkgs.agno;
          python3Packages-mcp = pkgs.python3.pkgs.mcp;

          # MCP servers from external inputs
          mcp-selenium = inputs.mcp-selenium.packages.${system}.default;
          mcp-nixos = inputs.mcp-nixos.packages.${system}.default;

          # VM configuration modules (not actual packages, but for reference)
          vm-config = ./vm-config.nix;
          agent-service = ./agent-service.nix;

          # Create an agent development servers package
          agent-servers = pkgs.mkMCPDevServers {
            name = "agent-vm-servers";
            shell = defaultShell;
          };

          # Default package is the agent-vm tool
          default = agent-vm;
        };

        # Apps for running the agent
        apps = rec {
          # Main agent VM app - this is the primary interface
          agent-vm = flake-utils.lib.mkApp {
            drv = pkgs.agent-vm;
          };

          # Direct agent execution (fallback)
          agent-direct = flake-utils.lib.mkApp {
            drv = pkgs.mkMCPDevServers {
              name = "agent-direct";
              shell = defaultShell;
            };
          };

          # Default app is the agent-vm tool
          default = agent-vm;

          # Convenience aliases for VM management
          create-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "create-vm";
              runtimeInputs = [ pkgs.agent-vm ];
              text = "exec agent-vm create \"$@\"";
            };
          };

          start-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "start-vm";
              runtimeInputs = [ pkgs.agent-vm ];
              text = "exec agent-vm start \"$@\"";
            };
          };

          stop-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "stop-vm";
              runtimeInputs = [ pkgs.agent-vm ];
              text = "exec agent-vm stop \"$@\"";
            };
          };

          shell-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "shell-vm";
              runtimeInputs = [ pkgs.agent-vm ];
              text = "exec agent-vm shell \"$@\"";
            };
          };
        };

        # Development shells
        devShells = {
          default = pkgs.mkShell {
            buildInputs = with pkgs; [
              # Development tools
              git
              nix
              nixpkgs-fmt

              # VM management
              agent-vm
              openssh
              qemu

              # MCP tools
              codemcp
              mcp-proxy
              mcp-language-server
              inputs.rescript-lsp.packages.${system}.default
              inputs.mcp-selenium.packages.${system}.default
              inputs.mcp-nixos.packages.${system}.default

              # Runtime tools
              curl
              python3
            ];

            shellHook = ''
              echo "🚀 Agent VM Development Environment"
              echo "📦 Available commands:"
              echo "  agent-vm create     - Create a new VM configuration"
              echo "  agent-vm start      - Start VM for current branch"
              echo "  agent-vm stop       - Stop VM"
              echo "  agent-vm shell      - Open shell in VM"
              echo "  agent-vm status     - Show VM status"
              echo "  agent-vm list       - List all VM configurations"
              echo "  agent-vm destroy    - Destroy VM configuration"
              echo ""
              echo "📖 Documentation: ./AGENT_ISOLATION.md"
              echo "📋 Task list: ./TODO.md"
            '';
          };

          # Minimal shell with just agent-vm
          minimal = pkgs.mkShell {
            buildInputs = with pkgs; [
              agent-vm
              git
              openssh
              qemu
            ];
          };
        };

        # Checks for CI/testing
        checks = {
          # Ensure agent-vm builds correctly
          agent-vm-build = pkgs.agent-vm;

          # Ensure all MCP packages build
          mcp-packages-build = pkgs.symlinkJoin {
            name = "mcp-packages";
            paths = with pkgs; [
              codemcp
              mcp-proxy
              mcp-language-server
              inputs.mcp-selenium.packages.${system}.default
              inputs.mcp-nixos.packages.${system}.default
            ];
          };

          # Python syntax check for agent-vm
          agent-vm-syntax = pkgs.runCommand "agent-vm-syntax-check"
            {
              buildInputs = [ pkgs.python3 ];
            } ''
            ${pkgs.python3}/bin/python -m py_compile ${./agent-vm.py}
            touch $out
          '';
        };

        # Formatter for the flake
        formatter = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            shellcheck.enable = true;
            deadnix.enable = true;
            programs.ruff.enable = true;
          };
        };
      }
    );
}
