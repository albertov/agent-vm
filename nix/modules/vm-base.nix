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
  };
  config = {
    services.qemuGuest.enable = true;
    boot.tmp.useTmpfs = true;

    # VM-specific configuration
    virtualisation = {
      memorySize = 1024 * 4;
      cores = 8; # 4 CPU cores
      diskSize = 1024 * 32; # 32GB disk
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
        [ closure ];
      writableStore = true;
      writableStoreUseTmpfs = false;
      useNixStoreImage = false;

      sharedDirectoriesVIO = {
        workspace = {
          # source gets injected by agent-vm
          # TODO: Make configurable
          source = "/home/alberto/src/agent-vm";
          target = "/var/lib/mcp-proxy/workspace";
        };
      };

      forwardPorts = [

        {
          from = "host";
          # TODO: Make configurable
          host.port = 8000;
          guest.port = config.services.mcp-proxy.port;
        } # MCP proxy
      ];

    };

    # Firewall configuration
    networking.firewall.enable = true;

    # TODO: Make configurable
    environment.systemPackages = with pkgs; [
      vim
      git
    ];

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
