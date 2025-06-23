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

    # Haskell.nix infrastructure
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs-haskellNix.follows = "haskellNix/nixpkgs-unstable";

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

  outputs = { self, nixpkgs, flake-utils, treefmt-nix, haskellNix, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import inputs.nixpkgs-haskellNix {
          inherit system;
          inherit (haskellNix) config;
          overlays = [
            # Haskell.nix overlay
            haskellNix.overlay
            # Agent overlay
            (import ./overlay.nix inputs)
          ];
        };

        # Default shell for development
        defaultShell = self.outputs.devShells.${system}.default;

      in
      {
        # Legacy packages for compatibility
        legacyPackages = pkgs;

        # Apps for running the agent
        apps = rec {
          # Main agent VM app (Python version) - for backward compatibility
          agent-vm-py = flake-utils.lib.mkApp {
            drv = pkgs.agent-vm-py;
          };

          # Haskell agent-vm app (new implementation)
          agent-vm = flake-utils.lib.mkApp {
            drv = pkgs.agent-vm.agent-vm.components.exes.agent-vm;
          };

          # Python integration test executable for comprehensive testing
          py-integration-test = flake-utils.lib.mkApp {
            drv = pkgs.py-integration-test;
          };

          # Direct agent execution (fallback)
          agent-direct = flake-utils.lib.mkApp {
            drv = pkgs.mkMCPDevServers {
              name = "agent-direct";
              shell = defaultShell;
            };
          };

          # Haskell integration test stub (to be implemented)
          integration-test = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "integration-test";
              text = ''
                echo "Haskell integration tests not yet implemented"
                exit 1
              '';
            };
          };

          # Default app is the agent-vm-py tool (for now)
          default = agent-vm-py;

          # Convenience aliases for VM management
          create-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "create-vm";
              runtimeInputs = [ pkgs.agent-vm-py ];
              text = "exec agent-vm-py create \"$@\"";
            };
          };

          start-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "start-vm";
              runtimeInputs = [ pkgs.agent-vm-py ];
              text = "exec agent-vm-py start \"$@\"";
            };
          };

          stop-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "stop-vm";
              runtimeInputs = [ pkgs.agent-vm-py ];
              text = "exec agent-vm-py stop \"$@\"";
            };
          };

          shell-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "shell-vm";
              runtimeInputs = [ pkgs.agent-vm-py ];
              text = "exec agent-vm-py shell \"$@\"";
            };
          };
        };

        # Development shells
        devShells = {
          default = pkgs.mkShell {
            inputsFrom = [ pkgs.agent-vm-py ];
            buildInputs = with pkgs; [

              # Runtime tools
              curl
              python3
              python3.pkgs.pytest

              # Integration testing
              py-integration-test
            ];

            shellHook = ''
              echo "🚀 Agent VM Development Environment"
              echo ""
              echo "📦 Available commands:"
              echo "  agent-vm-py create  - Create a new VM configuration (Python)"
              echo "  agent-vm-py start   - Start VM for current branch"
              echo "  agent-vm-py stop    - Stop VM"
              echo "  agent-vm-py shell   - Open shell in VM"
              echo "  agent-vm-py status  - Show VM status"
              echo "  agent-vm-py list    - List all VM configurations"
              echo "  agent-vm-py destroy - Destroy VM configuration"
              echo ""
              echo "  agent-vm           - Haskell version (in development)"
              echo ""
              echo "🧪 Testing:"
              echo "  py-integration-test - Run Python integration tests"
              echo "  integration-test    - Run Haskell integration tests (stub)"
              echo "  pytest tests/       - Run unit tests"
              echo ""
              echo "🛠️ Development shells:"
              echo "  nix develop .#haskell - Enter Haskell development environment"
              echo "  nix develop          - This shell (default)"
              echo ""
              echo "📖 Documentation: ./AGENT_ISOLATION.md"
              echo "📋 Task list: ./TODO.md"
            '';
          };

          # Minimal shell with just agent-vm-py
          minimal = pkgs.mkShell {
            buildInputs = with pkgs; [
              agent-vm-py
              git
              openssh
              qemu
            ];
          };

          # Haskell development shell
          haskell = pkgs.agent-vm.shellFor {
            tools = {
              cabal = {};
              haskell-language-server = {};
              hlint = {};
              hoogle = {};
            };
            buildInputs = with pkgs; [
              git
              openssh
              qemu
              curl
            ];
            shellHook = ''
              echo "🚀 Agent VM Haskell Development Environment"
              echo "📦 Available tools:"
              echo "  cabal              - Build tool"
              echo "  haskell-language-server - LSP server"
              echo "  hlint              - Linter"
              echo "  hoogle             - API search"
              echo ""
              echo "🔨 Build commands:"
              echo "  cabal build        - Build the project"
              echo "  cabal test         - Run tests"
              echo "  cabal run agent-vm - Run the executable"
              echo ""
              echo "📋 Task list: ./TODO.md"
            '';
          };
        };

        # Checks for CI/testing
        checks = {
          # Ensure agent-vm-py builds correctly
          agent-vm-py-build = pkgs.agent-vm-py;

          # Ensure agent-vm Haskell builds correctly
          agent-vm-build = pkgs.agent-vm.agent-vm.components.exes.agent-vm;

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

        };

        # Formatter for the flake
        formatter = treefmt-nix.lib.mkWrapper pkgs {
          projectRootFile = "flake.nix";
          programs = {
            nixpkgs-fmt.enable = true;
            shellcheck.enable = true;
            deadnix.enable = true;
            ruff.enable = true;
          };
        };
      }
    );
}
