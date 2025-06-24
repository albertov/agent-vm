{
  config,
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./mcp-proxy.nix
    ./selenium-server.nix
    ./virtiofs-qemu.nix
  ];
  options = {
    agent-vm = {
      tmpfs = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to use tmpfs for /tmp";
      };

      memorySize = lib.mkOption {
        type = lib.types.int;
        default = 1024 * 4;
        description = "VM memory size in MB";
      };

      cores = lib.mkOption {
        type = lib.types.int;
        default = 8;
        description = "Number of CPU cores for the VM";
      };

      diskSize = lib.mkOption {
        type = lib.types.int;
        default = 1024 * 32;
        description = "VM disk size in MB";
      };

      diskImage = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "./${config.system.name}.qcow2";
        description = "VM disk image file. null for a tmpfs root FS";
      };

      additionalPaths = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = [ ];
        description = "Additional paths to make available in the VM";
      };

      workspace = lib.mkOption {
        type = lib.types.str;
        description = "Source path for workspace shared directory";
      };

      port = lib.mkOption {
        type = lib.types.int;
        default = 8000;
        description = "Host port for forwarding to MCP proxy";
      };

      systemPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = with pkgs; [
          vim
          git
        ];
        description = "System packages to install";
      };

      uid = lib.mkOption {
        type = lib.types.int;
        default = 1000;
        description = "UID for MCP proxy user";
      };

      gid = lib.mkOption {
        type = lib.types.int;
        default = 100;
        description = "GID for MCP proxy user";
      };

      group = lib.mkOption {
        type = lib.types.str;
        default = "mcp-proxy";
        description = "Overrides group of mcp-proxy user. Useful when gid can't be used because of conflics";
      };

      shellEnv = lib.mkOption {
        type = lib.types.path;
        default = lib.extractShellEnv {
          inherit (config.agent-vm) flake shell;
        };
        description = '''
          The path to a file with the output of nix print-dev-env flake#shell
          '';
      };
      shell = lib.mkOption {
        type = lib.types.str;
        default = "default";
      };
      flake = lib.mkOption {
        type = lib.types.unspecified;
      };
    };
  };
  config = {
    services.qemuGuest.enable = true;
    boot.tmp.useTmpfs = config.agent-vm.tmpfs;

    # VM-specific configuration
    virtualisation = {
      memorySize = config.agent-vm.memorySize;
      cores = config.agent-vm.cores;
      diskSize = config.agent-vm.diskSize;
      diskImage = config.agent-vm.diskImage;
      graphics = false; # Headless for better performance
      mountHostNixStore = true;
      additionalPaths = config.agent-vm.additionalPaths;
      writableStore = true;
      writableStoreUseTmpfs = false;
      useNixStoreImage = false;

      sharedDirectoriesVIO = {
        workspace = {
          source = config.agent-vm.workspace;
          target = "/var/lib/mcp-proxy/workspace";
        };
      };

      forwardPorts = [
        {
          from = "host";
          host.port = config.agent-vm.port;
          guest.port = config.services.mcp-proxy.port;
        } # MCP proxy
      ];

    };

    # Firewall configuration
    networking.firewall.enable = true;

    environment.systemPackages = config.agent-vm.systemPackages;

    # Security hardening
    security = {
      allowUserNamespaces = true; # Chrome needs user namespaces for its sandbox
      lockKernelModules = true;
      protectKernelImage = true;
      # Configure passwordless sudo for dev user for systemctl commands
      sudo = {
        enable = true;
        wheelNeedsPassword = false;
      };
    };

    # Nix sandbox configuration
    nix.settings.sandbox = false;
    nix.settings.experimental-features = [
      "nix-command"
      "flakes"
    ];
    nix.settings.trusted-users = [ "mcp-proxy" ];

    # Enable and configure the agent service
    services.getty.autologinUser = "mcp-proxy";
    users.users.mcp-proxy.extraGroups = [ "wheel" ];
    users.users.mcp-proxy.packages = with pkgs; [
      vim
      tmux
      git
      nix
      coreutils
    ];
    system.activationScripts.mcp-proxy-env =
      ''
        MCP_HOME_DIR="${config.users.users.mcp-proxy.home}"
        mkdir -p "$MCP_HOME_DIR"
        cat > "$MCP_HOME_DIR"/.profile << 'EOF'
        if [ -z "$MCP_SHELL_INIT" ]; then
          export MCP_SHELL_INIT=YES
          pushd ${config.users.users.mcp-proxy.home}/workspace
          source ${config.agent-vm.shellEnv}
          popd
        fi
        EOF
      '';

    services.mcp-proxy = {
      enable = true;
      openFirewall = true;
      host = "0.0.0.0";
      port = 8000;
      uid = config.agent-vm.uid;
      gid = config.agent-vm.gid;
      group = config.agent-vm.group;
      shellEnv = lib.mkDefault config.agent-vm.shellEnv;
      namedServers.codemcp = {
        enabled = lib.mkDefault true;
        command = lib.mkDefault "codemcp";
        runtimeInputs = [ pkgs.codemcp ];
      };
      namedServers.selenium = {
        enabled = lib.mkDefault config.services.selenium-server.enable;
        command = lib.mkDefault "mcp-selenium-hs";
        runtimeInputs = [ pkgs.mcp-selenium ];
      };
      allowOrigins = [ "https://claude.ai" ];
    };

    # System state version
    system.stateVersion = "25.05";
  };
}
