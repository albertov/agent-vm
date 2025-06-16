# agent-service.nix
{ config, lib, pkgs, ... }:

let
  cfg = config.services.agent-mcp;
in
{
  options.services.agent-mcp = {
    enable = lib.mkEnableOption "Agent MCP service";

    name = lib.mkOption {
      type = lib.types.str;
      default = "agent";
      description = "Agent exe name";
    };

    shell = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "The devshell derivation to inject in agent's environment";
    };

    pkgs = lib.mkOption {
      description = "The package set";
      default = pkgs;
    };

    allowOrigin = lib.mkOption {
      type = lib.types.str;
      default = "https://claude.ai";
      description = "Allowed origin for mcp-proxy";
    };

    user = lib.mkOption {
      type = lib.types.str;
      default = "agent";
      description = "User to run the agent service as";
    };

    group = lib.mkOption {
      type = lib.types.str;
      default = "agent";
      description = "Group to run the agent service as";
    };

    workspaceDir = lib.mkOption {
      type = lib.types.path;
      default = "/workspace";
      description = "Path to the workspace directory";
    };

    port = lib.mkOption {
      type = lib.types.port;
      default = 8000;
      description = "Port for the MCP proxy service";
    };

    extraEnvironment = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      default = { };
      description = "Extra environment variables for the service";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.agent-mcp =
      let
        # Create the MCP dev servers with the configured shell
        devServers = pkgs.mkMCPDevServers {
          inherit (cfg) name pkgs;
          # Use the provided shell or create a minimal default
          shell = if cfg.shell != null then cfg.shell else pkgs.mkShell {
            buildInputs = with pkgs; [ git curl ];
          };
        };
      in
      {
        description = "Agent MCP Service";
        after = [ "network.target" "multi-user.target" ];
        wantedBy = [ "multi-user.target" ];

        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Group = cfg.group;
          WorkingDirectory = cfg.workspaceDir;

          # Security hardening - minimal capabilities
          NoNewPrivileges = true;
          ProtectSystem = "strict";
          ProtectHome = true;
          ProtectKernelTunables = true;
          ProtectKernelModules = true;
          ProtectControlGroups = true;
          RestrictSUIDSGID = true;
          RestrictRealtime = true;
          RestrictNamespaces = true;
          LockPersonality = true;
          MemoryDenyWriteExecute = false; # May be needed for some MCP tools
          RemoveIPC = true;

          # Filesystem access
          ReadWritePaths = [ cfg.workspaceDir ];
          PrivateTmp = true;
          PrivateDevices = true;
          ProtectClock = true;

          # Network access (required for MCP proxy)
          PrivateNetwork = false;

          # Restart policy
          Restart = "always";
          RestartSec = "5s";
          StartLimitBurst = 3;

          # Environment
          Environment = [
            "MCP_PROXY_PORT=${toString cfg.port}"
            "MCP_PROXY_HOST=0.0.0.0" # We're in a VM, this is safe
            "ALLOW_ORIGIN=${cfg.allowOrigin}"
            "WORKSPACE_DIR=${cfg.workspaceDir}"
          ] ++ lib.mapAttrsToList (name: value: "${name}=${value}") cfg.extraEnvironment;

          # Start the MCP services
          ExecStart = "${pkgs.writeShellScript "start-agent-mcp" ''
            set -euo pipefail

            # Ensure workspace directory exists and is accessible
            if [ ! -d "${cfg.workspaceDir}" ]; then
              echo "Error: Workspace directory ${cfg.workspaceDir} does not exist"
              exit 1
            fi
            # Start agent produced by mkMCPDevServers
            exec ${devServers}/bin/start-agent
          ''}";

          # Health check
          ExecStartPost = "${pkgs.writeShellScript "check-agent-health" ''
            # Wait for service to be ready
            for i in {1..30}; do
              if ${pkgs.curl}/bin/curl -f http://localhost:${toString cfg.port}/health > /dev/null 2>&1; then
                echo "Agent MCP service is ready"
                exit 0
              fi
              sleep 1
            done
            echo "Agent MCP service failed to become ready"
            exit 1
          ''}";

          # Graceful shutdown
          KillMode = "mixed";
          KillSignal = "SIGTERM";
          TimeoutStopSec = "30s";
        };
      };

    # Ensure the service user exists (only if not using system users)
    users.users = lib.mkIf (cfg.user != "root" && cfg.user != "dev") {
      ${cfg.user} = {
        isSystemUser = true;
        group = cfg.group;
        description = "Agent MCP service user";
      };
    };

    users.groups = lib.mkIf (cfg.group != "root" && cfg.group != "dev") {
      ${cfg.group} = { };
    };
  };
}
