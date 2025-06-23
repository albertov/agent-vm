{
  self,
  config,
  inputs,
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
    "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
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

      workspaceSource = lib.mkOption {
        type = lib.types.str;
        default = "/home/alberto/src/agent-vm";
        description = "Source path for workspace shared directory";
      };

      hostPort = lib.mkOption {
        type = lib.types.int;
        default = 8000;
        description = "Host port for forwarding to MCP proxy";
      };

      systemPackages = lib.mkOption {
        type = lib.types.listOf lib.types.package;
        default = with pkgs; [ vim git ];
        description = "System packages to install";
      };

      mcpProxy = {
        port = lib.mkOption {
          type = lib.types.int;
          default = 8000;
          description = "MCP proxy port";
        };

        uid = lib.mkOption {
          type = lib.types.int;
          default = 1000;
          description = "UID for MCP proxy user";
        };

        group = lib.mkOption {
          type = lib.types.str;
          default = "users";
          description = "Group for MCP proxy user";
        };

        shell = lib.mkOption {
          type = lib.types.package;
          default = self.devShells."${pkgs.system}".default;
          description = "Shell environment for MCP proxy";
        };
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
          systemChecks = self.checks.${pkgs.system} or { };
          packages = self.packages.${pkgs.system} or { };
          apps = self.apps.${pkgs.system} or { };
          allItems =
            allInputs shell
            ++ (
              if pkgs ? hixProject then
                [
                  pkgs.hixProject.roots
                  pkgs.hixProject.plan-nix
                ]
              else
                [ ]
            )
            ++ (pkgs.lib.concatMap allInputs (pkgs.lib.attrValues systemChecks))
            ++ (pkgs.lib.concatMap allInputs (pkgs.lib.attrValues packages))
            ++ (map (x: x.program) (pkgs.lib.attrValues apps));

          closure = pkgs.closureInfo { rootPaths = allItems; };
        in
        [ closure ] ++ config.agent-vm.additionalPaths;
      writableStore = true;
      writableStoreUseTmpfs = false;
      useNixStoreImage = false;

      sharedDirectoriesVIO = {
        workspace = {
          source = config.agent-vm.workspaceSource;
          target = "/var/lib/mcp-proxy/workspace";
        };
      };

      forwardPorts = [

        {
          from = "host";
          host.port = config.agent-vm.hostPort;
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
      # TODO: Make configurable
      port = 8000;
      # TODO: Make configurable
      uid = 1000;
      # TODO: Make configurable
      group = "users";
      # TODO: Make configurable
      shell = lib.mkDefault self.devShells."${pkgs.system}".default;
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
