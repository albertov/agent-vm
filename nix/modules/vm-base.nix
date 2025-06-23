{
  config,
  pkgs,
  lib,
  ...
}:
let
  allInputs =
    p:
    (p.buildInputs or [ ])
    ++ (p.nativeBuildInputs or [ ])
    ++ (p.propagatedBuildInputs or [ ])
    ++ (p.propagatedNativeBuildInputs or [ ]);
in
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
        default = null;
        description = "Overrides group of mcp-proxy user. Useful when gid can't
        be used because of conflics";
      };

      shell = lib.mkOption {
        type = lib.types.package;
        description = "Shell environment for MCP proxy";
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
      graphics = false; # Headless for better performance
      mountHostNixStore = true;
      additionalPaths =
        let
          shell = config.services.mcp-proxy.shell;
          allItems =
            allInputs shell
            ++ config.agent-vm.additionalPaths
            ++ (
              if pkgs ? hixProject then
                [
                  pkgs.hixProject.roots
                  pkgs.hixProject.plan-nix
                ]
              else
                [ ]
            );

          closure = pkgs.closureInfo { rootPaths = allItems; };
        in
        [ closure ];
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
    users.users.mcp-proxy.packages = allInputs config.services.mcp-proxy.shell;
    services.mcp-proxy = {
      enable = true;
      openFirewall = true;
      host = "0.0.0.0";
      port = 8000;
      uid = config.agent-vm.uid;
      gid = config.agent-vm.gid;
      group = config.agent-vm.group;
      shell = lib.mkDefault config.agent-vm.shell;
      namedServers.codemcp = {
        enabled = lib.mkDefault true;
        command = lib.mkDefault "${pkgs.codemcp}/bin/codemcp";
      };
      namedServers.selenium = {
        enabled = lib.mkDefault config.services.selenium-server.enable;
        command = lib.mkDefault "${pkgs.mcp-selenium}/bin/mcp-selenium-hs";
      };
      allowOrigins = [ "https://claude.ai" ];
    };

    # System state version
    system.stateVersion = "25.05";
  };
}
