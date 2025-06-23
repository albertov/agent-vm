# vm-config.nix
{ config, pkgs, lib, ... }:
{
  # VM-specific configuration
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096; # 4GB RAM for development work
      cores = 4; # 4 CPU cores
      diskSize = 4096; # 4GB disk
      graphics = false; # Headless for better performance

      # High-performance workspace sharing via VirtioFS
      sharedDirectories = {
        workspace = {
          source = "/workspace";
          target = "/workspace";
          securityModel = "mapped-xattr";
        };
      };

      # Port forwarding for MCP proxy
      forwardPorts = [
        { from = "host"; host.port = 8000; guest.port = 8000; } # MCP proxy
        { from = "host"; host.port = 2222; guest.port = 22; } # SSH access
      ];

      # Optimized QEMU options for VirtioFS performance
      qemu.options = [
        "-object memory-backend-memfd,id=mem,size=4G,share=on"
        "-numa node,memdev=mem"
      ];
    };
  };


  # Development user configuration
  users.users.dev = {
    isNormalUser = true;
    group = "dev";
    extraGroups = [ "wheel" ]; #TODO: Assess if necessary
    shell = pkgs.bash;
    # SSH key will be injected dynamically during VM startup
    openssh.authorizedKeys.keys = [
      # Ephemeral SSH public key will be added here during VM creation
    ];
    # Make essential development tools available to the user
    packages = with pkgs; [
      git
      nix
      openssh
      curl
      python3
      # MCP tools
      codemcp
      mcp-proxy
      mcp-language-server
      rescript-language-server
    ];
  };

  # Define the dev group
  users.groups.dev = {};

  # SSH access for development with secure key-based authentication
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false; # Disable password auth for security
      PubkeyAuthentication = true; # Enable key-based authentication
      PermitRootLogin = "no";
      AuthenticationMethods = "publickey";
    };
  };

  # Firewall configuration
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 config.services.agent-mcp.port ]; # SSH and MCP proxy
  };

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

  # Import the agent service module
  imports = [ ./agent-service.nix ];

  # Enable and configure the agent service
  services.agent-mcp = {
    enable = true;
    user = "dev";
    group = "dev";
    workspaceDir = "/workspace";
    # Pass the development shell with all MCP tools using mkMCPDevServers
    shell = pkgs.mkShell {
      buildInputs = with pkgs; [
        # Development tools
        git
        nix
        openssh
        qemu
        curl
        python3

        # MCP tools (should match what's in mkMCPDevServers)
        codemcp
        mcp-proxy
        mcp-language-server
        rescript-language-server
      ];
      shellHook = ''
        echo "🚀 Agent MCP Development Environment in VM"
      '';
    };
    # These would be overrided in a module added by the create admin command
    # which imports this base config
    port = 8000;
    allowOrigin = "https://claude.ai";
  };

  # System state version
  system.stateVersion = "24.11";
}
