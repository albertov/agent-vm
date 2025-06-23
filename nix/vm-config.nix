# vm-config.nix
{
  self,
  config,
  inputs,
  pkgs,
  ...
}:
{
  imports = [
    "${inputs.nixpkgs}/nixos/modules/virtualisation/qemu-vm.nix"
    ./modules/mcp-proxy.nix
    ./modules/selenium-server.nix
  ];
  # VM-specific configuration
  virtualisation = {
    memorySize = 4096; # 4GB RAM for development work
    cores = 4; # 4 CPU cores
    diskSize = 4096; # 4GB disk
    graphics = false; # Headless for better performance

    # High-performance workspace sharing via VirtioFS
    sharedDirectories = {
      workspace = {
        # source gets injected by agent-vm
        source = "/home/alberto/src/agent-vm";
        target = "/var/lib/mcp-proxy/workspace";
        securityModel = "mapped-xattr";
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

    # Optimized QEMU options for VirtioFS performance
    qemu.options = [
      "-object memory-backend-memfd,id=mem,size=4G,share=on"
      "-numa node,memdev=mem"
    ];
  };

  # Firewall configuration
  networking.firewall.enable = true;

  # Optimize for VM environment
  services.qemuGuest.enable = true;

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
  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  services.selenium-server.enable = true;

  # Enable and configure the agent service
  services.getty.autologinUser = "mcp-proxy";
  users.users.mcp-proxy.extraGroups = [ "wheel" ];
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
