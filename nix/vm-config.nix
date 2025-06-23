# vm-config.nix
{
  self,
  config,
  inputs,
  pkgs,
  ...
}:
let
  allInputs = p:
       (p.buildInputs or [ ])
    ++ (p.nativeBuildInputs or [ ])
    ++ (p.propagatedBuildInputs or [ ])
    ++ (p.propagatedNativeBuildInputs or [ ]);
in
{
  imports = [
    "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
    ./modules/mcp-proxy.nix
    ./modules/selenium-server.nix
  ];
  # VM-specific configuration
  virtualisation = {
    memorySize = 1024 * 16; # 16GB RAM for development work
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
          [
            config.system.build.toplevel
            self
          ]
          ++ allInputs shell
          ++ (if pkgs?hixProject
            then [
              pkgs.hixProject.roots
              pkgs.hixProject.plan-nix
            ]
            else [])
          ++ (pkgs.lib.concatMap allInputs (pkgs.lib.attrValues systemChecks))
          ++ (pkgs.lib.concatMap allInputs (pkgs.lib.attrValues packages))
          ++ (map (x: x.program) (pkgs.lib.attrValues apps))
          ;

        closure = pkgs.closureInfo { rootPaths = allItems; };
      in
      [ closure ];
    writableStore = true;
    writableStoreUseTmpfs = false;
    useNixStoreImage = false;

    # High-performance workspace sharing via VirtioFS
    sharedDirectories = {
      workspace = {
        # source gets injected by agent-vm
        source = "/home/alberto/src/agent-vm";
        target = "/var/lib/mcp-proxy/workspace";
        securityModel = "none";
      };
    };

    # Port forwarding for MCP proxy
    forwardPorts = [
      {
        from = "host";
        host.port = 8000;
        guest.port = config.services.mcp-proxy.port;
      } # MCP proxy
    ];

  };

  # Firewall configuration
  networking.firewall.enable = true;

  # Optimize for VM environment
  services.qemuGuest.enable = true;

  environment.systemPackages = with pkgs; [
    vim
    git
    codemcp
  ];

  # Security hardening
  security = {
    allowUserNamespaces = false;
    lockKernelModules = true;
    protectKernelImage = true;
    # Configure passwordless sudo for dev user for systemctl commands
    sudo = {
      enable = true;
      wheelNeedsPassword = false;
    };
  };

  # Disable sandbox to avoid conflict with security.allowUserNamespaces = false
  nix.settings.sandbox = false;
  nix.settings.experimental-features = [
    "nix-command"
    "flakes"
  ];
  nix.settings.trusted-users = [ "mcp-proxy" ];

  # Configure host nix store as binary cache
  nix.settings.substituters = [
    "file:///nix/store"  # Use mounted host store as primary cache
    "https://cache.nixos.org/"  # Fallback to public cache
    "https://cache.iog.io"  # Additional cache from flake config
    "https://nixcache.plowtech.net:9876/"  # Additional cache from flake config
  ];
  # Trust the host store implicitly since it's mounted from host
  nix.settings.trusted-substituters = [
    "file:///nix/store"
  ];
  nix.settings.trusted-public-keys = [
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    "jenkins.plowtech.net-1:7MnrDY0TzJTvmaSlRT25noN7qbvqRnLoLOqaxMBNckI="
  ];

  services.selenium-server.enable = true;

  # Enable and configure the agent service
  services.getty.autologinUser = "mcp-proxy";
  users.users.mcp-proxy.extraGroups = [ "wheel" ];
  users.users.mcp-proxy.packages = allInputs config.services.mcp-proxy.shell;
  services.mcp-proxy = {
    enable = true;
    openFirewall = true;
    # These would be overrided in a module added by the create admin command
    # which imports this base config
    port = 8000;
    host = "0.0.0.0";
    shell = self.devShells."${pkgs.system}".default;
    namedServers.codemcp = {
      enabled = true;
      command = "${pkgs.codemcp}/bin/codemcp";
    };
    allowOrigins = [ "https://claude.ai" ];
  };

  # System state version
  system.stateVersion = "25.05";
}
