{
  description = "Agent VM - Isolated MCP agent development environment using QEMU VMs";

  nixConfig = {
    bash-prompt = "agent-vm \\[\\e[0m\\]\\w \\[\\e[0;1m\\]λ \\[\\e[0m\\]";
    extra-substituters = [
    ];
    extra-trusted-public-keys = [
    ];
    allow-import-from-derivation = true;
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
            (final: _prev: {
              inherit hoogle;
              hixProject = final.haskell-nix.hix.project {
                src = builtins.path {
                  path = ./agent-vm;
                  name = "source";
                };
                evalSystem = "x86_64-linux";
              };
            })
            # Agent overlay
            (import ./nix/overlay.nix inputs)
          ];
        };
        flake = pkgs.hixProject.flake { };
        hoogleEnv = pkgs.hixProject.ghcWithHoogle (
          _:
          builtins.attrValues (
            pkgs.lib.filterAttrs (_: p: p.isLocal or false && p.components ? library) pkgs.hixProject.hsPkgs
          )
        );
        hoogle = pkgs.writeShellApplication {
          name = "hoogle";
          runtimeInputs = [ hoogleEnv ];
          text = ''
            exec hoogle "$@"
          '';
        };

        # Default shell for development
        defaultShell = self.outputs.devShells.${system}.default;

      in
      (pkgs.lib.recursiveUpdate flake {
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
            drv = pkgs.hixProject.hsPkgs.agent-vm.components.exes.agent-vm;
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
          default = agent-vm;

          # Convenience aliases for VM management
          create-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "create-vm";
              runtimeInputs = [ pkgs.agent-vm-py ];
              text = "exec agent-vm-py create \"$@\"";
            };
          };
          update-materialization = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "update-materialization";
              runtimeInputs = with pkgs; [
                nix
                git
                rsync
              ];
              text = ''
                set -euo pipefail

                echo "🔄 Updating Nix materialization..."

                PLAN_RESULT="$(mktemp -d)/plan"
                HIX="$(pwd)/agent-vm/nix/hix.nix"

                # Backup the original hix.nix
                cp "$HIX" "$HIX".backup
                trap 'mv $HIX.backup $HIX; rm $PLAN_RESULT' EXIT

                # Step 1: Temporarily disable materialization by commenting out the line
                echo "📝 Temporarily disabling materialization..."
                sed -i 's/materialized = /# materialized = # Temporarily disabled/' "$HIX"

                # Step 2: Try to build the plan-nix (this will use IFD but generate what we need)
                echo "🏗️ Building project plan..."
                nix build .#hixProject.plan-nix -o "$PLAN_RESULT"

                # Step 3: Remove old materialized files and copy new ones
                echo "📁 Updating materialized files..."
                rm -rf nix/materialized
                mkdir -p nix/materialized
                rsync -a "$PLAN_RESULT"/ nix/materialized/
                chmod -R u+w nix/materialized

                # Step 4: Restore the original $HIX (re-enable materialization)
                echo "🔧 Re-enabling materialization..."
                mv "$HIX".backup "$HIX"
                trap - EXIT

                # Step 5: Test that it works
                echo "🧪 Testing materialization..."
                git add -f nix/materialized
                if nix flake check; then
                  echo "✅ Flake check passed"
                  # Step 6: Commit the materialized files
                  echo "📝 Committing materialized files..."
                else
                  git restore --staged nix/materialized
                  git checkout nix/materialized
                  echo "⚠️ Flake check had issues"
                fi


                if git diff --cached --quiet; then
                  echo "ℹ️ No changes to commit - materialization was already up to date"
                else
                  git commit -m "feat: update materialized nix files

                This updates the materialized files to match the current project
                dependencies, eliminating the need for import-from-derivation (IFD)
                during evaluation."
                  echo "✅ Committed materialized files"
                fi

                echo "🎉 Materialization updated successfully!"
                echo ""
                echo "The materialized files contain pre-computed Nix expressions that represent"
                echo "your Haskell dependencies, eliminating the need for import-from-derivation"
                echo "during evaluation."
              '';
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
          settings.global.excludes = [ "nix/materialized/**" ];
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            programs.ormolu.enable = true;
            programs.hlint.enable = true;
            shellcheck.enable = true;
            deadnix.enable = true;
            ruff.enable = true;
          };
        };
      })
      // {
        # These require IFD and we don't want that
        hydraJobs = { };
      }
    );
}
