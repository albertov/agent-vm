{
  description = "Agent VM - Isolated MCP agent development environment using QEMU VMs";

  nixConfig = {
    bash-prompt = "agent-vm \\[\\e[0m\\]\\w \\[\\e[0;1m\\]λ \\[\\e[0m\\]";
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };

  inputs = {
    # Core inputs
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    # Haskell.nix infrastructure
    haskellNix.url = "github:input-output-hk/haskell.nix/07980c9354842dd3858d124ccde54bf4fa010ecf";

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

  outputs =
    {
      nixpkgs,
      self,
      flake-utils,
      treefmt-nix,
      haskellNix,
      ...
    }@inputs:
    let
      overlays = [
        # Haskell.nix overlay
        haskellNix.overlay
        (final: _prev: {
          projectStatic = final.hixProject.projectCross.musl64;
          hoogleEnv = final.hixProject.ghcWithHoogle (
            _:
            builtins.attrValues (
              final.lib.filterAttrs (_: p: p.isLocal or false && p.components ? library) final.hixProject.hsPkgs
            )
          );
          hoogle = final.writeShellApplication {
            name = "hoogle";
            runtimeInputs = [ final.hoogleEnv ];
            text = ''
              exec hoogle "$@"
            '';
          };
          agent-vm-sh = final.writeShellApplication {
            name = "agent-vm";
            runtimeInputs = [
              final.nix
              final.git
            ];
            text = ''
              # Parse command line arguments
              CONFIG=""
              FLAKE=""
              DEVSHELL="default"
              PORT=8000
              WORKSPACE="$(pwd)"
              MEMORY_SIZE=4
              CORES=2
              DISK_SIZE=4

              # Show usage
              usage() {
                echo "Usage: $0 [OPTIONS]"
                echo ""
                echo "Options:"
                echo "  --config PATH      VM configuration file (optional)"
                echo "  --flake FLAKE      Flake to use (default: current git repo)"
                echo "  --devshell NAME    Development shell to use (default: $DEVSHELL)"
                echo "  --port PORT        Port for the VM (default: $PORT)"
                echo "  --workspace PATH   Workspace directory (default: current directory)"
                echo "  --memory SIZE      VM memory size in GB (default: $MEMORY_SIZE)"
                echo "  --cores NUM        Number of CPU cores (default: $CORES)"
                echo "  --disk SIZE        VM disk size in GB (default: $DISK_SIZE)"
                echo "  -h, --help         Show this help message"
                exit 0
              }

              # Parse arguments
              while [[ $# -gt 0 ]]; do
                case $1 in
                  --config)
                    CONFIG="$2"
                    shift 2
                    ;;
                  --flake)
                    FLAKE="$2"
                    shift 2
                    ;;
                  --devshell)
                    DEVSHELL="$2"
                    shift 2
                    ;;
                  --port)
                    PORT="$2"
                    shift 2
                    ;;
                  --workspace)
                    WORKSPACE="$2"
                    shift 2
                    ;;
                  --memory)
                    MEMORY_SIZE="$2"
                    shift 2
                    ;;
                  --cores)
                    CORES="$2"
                    shift 2
                    ;;
                  --disk)
                    DISK_SIZE="$2"
                    shift 2
                    ;;
                  -h|--help)
                    usage
                    ;;
                  *)
                    echo "Unknown option: $1"
                    usage
                    ;;
                esac
              done

              # If FLAKE is not provided, use the current git repo
              if [ "$FLAKE" == "" ]; then
                REPO_ROOT="$(git rev-parse --show-toplevel)"
                BRANCH=$(git branch --show-current)
                FLAKE="git+file://$REPO_ROOT?ref=$BRANCH"
              fi

              SYSTEM="$(nix eval --impure --expr builtins.currentSystem)"
              exec nix run --impure --show-trace --expr "
                let self = builtins.getFlake \"${self}\";
                    flake = builtins.getFlake \"$FLAKE\";
                    shell = flake.devShells.$SYSTEM.$DEVSHELL;
                    mkVM = mods: (self.lib.$SYSTEM.mk-agent-vm mod).config.system.build.vmWithVirtioFS;
                in mkVM
                    [$CONFIG
                      { agent-vm = {
                          inherit shell;
                          port=$PORT;
                          memorySize=1024 * $MEMORY_SIZE;
                          diskSize=1024 * $DISK_SIZE;
                          cores=$CORES;
                          uid=$(id -u);
                          group=\"$(id -n -g)\";
                          workspace=\"$WORKSPACE\";
                        };
                      }
                    ]
                "
            '';
          };
          # statically-linked exes
          agent-vm = final.projectStatic.getComponent "agent-vm:exe:agent-vm";
          agent-vm-test = final.projectStatic.getComponent "agent-vm:exe:agent-vm-test";
          hixProject = final.haskell-nix.hix.project {
            # We clean the source to avoid spurious recompiles
            src = final.lib.sources.cleanSourceWith rec {
              src = builtins.path {
                path = ./.;
                name = "source";
              };
              filter =
                path: _type:
                let
                  baseName = baseNameOf path;
                  # Get relative path from source root
                  relativePath = final.lib.removePrefix (toString src + "/") (toString path);
                in
                (
                  # Don't traverse into excluded directories
                  !(final.lib.hasPrefix "cabal.project.local" relativePath)
                  &&
                    # no bash scripts except test fixtures
                    !(final.lib.hasSuffix ".sh" baseName && !(final.lib.hasPrefix "test/fixtures/" relativePath))
                  && !(final.lib.hasSuffix ".md" baseName)
                  &&
                    # non-haskell related files
                    !(builtins.elem baseName [
                      "codemcp.toml"
                      "flake.nix"
                      "flake.lock"
                    ])
                  # && (relativePath == traceShowId relativePath)
                );
            };
            evalSystem = "x86_64-linux";
          };
        })
        # Agent overlay
        (import ./nix/overlay.nix inputs)
      ];
    in
    {
      nixosConfigurations = {
        agent-vm = self.lib.x86_64-linux.mk-agent-vm [ ./nix/vm-config.nix ];
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        # traceShowId = x: builtins.trace "Debug: ${toString x}" x;
        pkgs = import inputs.nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.hixProject.flake { };

        # Default shell for development
        defaultShell = self.outputs.devShells.${system}.default;

      in
      (pkgs.lib.recursiveUpdate flake {
        # Legacy packages for compatibility
        legacyPackages = pkgs;

        lib = {
          mk-agent-vm =
            mods:
            (inputs.nixpkgs.lib.nixosSystem {
              modules = [
                "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
                {
                  nixpkgs = {
                    inherit overlays;
                    inherit (haskellNix) config;
                    inherit (pkgs) system;
                  };
                }
                ./nix/modules/vm-base.nix
              ] ++ mods;
            });
        };

        # Apps for running the agent
        apps = rec {
          # Haskell agent-vm app (new implementation)
          agent-vm = flake-utils.lib.mkApp {
            drv = pkgs.agent-vm-sh;

          };

          # Haskell integration test stub (to be implemented)
          integration-test = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "integration-test";
              runtimeInputs = [ pkgs.agent-vm-test ];
              text = ''
                exec agent-vm-test "$@"
              '';
            };
          };

          # Direct agent execution (fallback)
          agent-direct = flake-utils.lib.mkApp {
            drv = pkgs.mkMCPDevServers {
              name = "agent-direct";
              shell = defaultShell;
            };
          };

          default = agent-vm;

          # Convenience aliases for VM management
          create-vm = flake-utils.lib.mkApp {
            drv = pkgs.writeShellApplication {
              name = "create-vm";
              runtimeInputs = [ pkgs.agent-vm ];
              text = "exec agent-vm create \"$@\"";
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

                # Backup the original hix.nix
                cp nix/hix.nix nix/hix.nix.backup
                trap 'mv nix/hix.nix.backup nix/hix.nix; rm $PLAN_RESULT' EXIT

                # Step 1: Temporarily disable materialization by commenting out the line
                echo "📝 Temporarily disabling materialization..."
                sed -i 's/materialized = \.\/materialized;/# materialized = \.\/materialized; # Temporarily disabled/' nix/hix.nix

                # Step 2: Try to build the plan-nix (this will use IFD but generate what we need)
                echo "🏗️ Building project plan..."
                nix build .#hixProject.plan-nix -o "$PLAN_RESULT"

                # Step 3: Remove old materialized files and copy new ones
                echo "📁 Updating materialized files..."
                rm -rf nix/materialized
                mkdir -p nix/materialized
                rsync -a "$PLAN_RESULT"/ nix/materialized/
                chmod -R u+w nix/materialized

                # Step 4: Restore the original hix.nix (re-enable materialization)
                echo "🔧 Re-enabling materialization..."
                mv nix/hix.nix.backup nix/hix.nix
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

        # Checks for CI/testing
        checks = {
          agent-vm-spec = pkgs.hixProject.hsPkgs.agent-vm.checks.spec.overrideAttrs (old: {
            buildInputs = old.buildInputs ++ [ pkgs.bash ];
          });

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
          settings.global.excludes = [
            "nix/materialized/**"
          ];
          projectRootFile = "flake.nix";
          programs = {
            nixfmt.enable = true;
            ormolu.enable = true;
            hlint.enable = true;
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
