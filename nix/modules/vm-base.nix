{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.agent-vm;
in
{
  imports = [
    ./mcp-proxy.nix
    ./selenium-server.nix
    ./virtiofs-qemu.nix
  ];
  options = {
    agent-vm = {
      name = lib.mkOption {
        type = lib.types.str;
        default = "agent-vm";
        description = "The VM name";
      };
      tmpfs = lib.mkOption {
        type = lib.types.bool;
        default = true;
        description = "Whether to use tmpfs for /tmp";
      };

      memorySize = lib.mkOption {
        type = lib.types.int;
        default = 4;
        description = "VM memory size in GB";
      };

      cores = lib.mkOption {
        type = lib.types.int;
        default = 8;
        description = "Number of CPU cores for the VM";
      };

      diskSize = lib.mkOption {
        type = lib.types.int;
        default = 32;
        description = "VM disk size in GB";
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

      serialSocket = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to the serial console socket. If null console won't be on socket but on console";
      };

      pidFile = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Path to the pid file. If null qemu won't daemonize";
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
        description = ''
          The path to a file with the output of nix print-dev-env flake#shell
        '';
      };
    };
  };
  config = {
    services.qemuGuest.enable = true;
    boot.tmp.useTmpfs = cfg.tmpfs;
    networking.hostName = cfg.name;

    # Serial console on the serialSocket
    systemd.services."serial-getty@ttyS0" = {
      enable = cfg.serialSocket != null;
      serviceConfig = {
        Restart = "always";
      };
    };

    # VM-specific configuration
    virtualisation = {
      cores = cfg.cores;
      memorySize = cfg.memorySize * 1024;
      diskSize = cfg.diskSize * 1024;
      diskImage = cfg.diskImage;
      graphics = false; # Headless for better performance
      mountHostNixStore = true;
      additionalPaths = cfg.additionalPaths;
      writableStore = true;
      writableStoreUseTmpfs = false;
      useNixStoreImage = false;
      qemu.options = (lib.optional (cfg.serialSocket != null)
        "-serial unix:${cfg.serialSocket},server,nowait")
        ++ (lib.optional (cfg.pidFile != null)
        "-deamonize -pidfile ${cfg.pidFile}");


      sharedDirectoriesVIO = {
        workspace = {
          source = cfg.workspace;
          target = "/var/lib/mcp-proxy/workspace";
        };
      };

      forwardPorts = [
        {
          from = "host";
          host.port = cfg.port;
          guest.port = config.services.mcp-proxy.port;
        }
      ];

    };

    # Firewall configuration
    networking.firewall.enable = true;

    environment.systemPackages = cfg.systemPackages;

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
    system.activationScripts.mcp-proxy-env = ''
      MCP_HOME_DIR="${config.users.users.mcp-proxy.home}"
      mkdir -p "$MCP_HOME_DIR"
      cat > "$MCP_HOME_DIR"/.profile << 'EOF'
      if [ -z "$MCP_SHELL_INIT" ]; then
        export MCP_SHELL_INIT=YES
        pushd ${config.users.users.mcp-proxy.home}/workspace
        source ${cfg.shellEnv}
        popd
      fi
      EOF
    '';

    services.mcp-proxy = {
      enable = true;
      openFirewall = true;
      host = "0.0.0.0";
      port = 8000;
      uid = cfg.uid;
      gid = cfg.gid;
      group = cfg.group;
      shellEnv = lib.mkDefault cfg.shellEnv;
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
