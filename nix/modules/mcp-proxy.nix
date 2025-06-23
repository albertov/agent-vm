{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.services.mcp-proxy;

  # Submodule for named server configuration
  namedServerOptions = {
    options = {
      enabled = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable this named server.";
      };

      command = mkOption {
        type = types.str;
        description = "The command to execute for the stdio server.";
        example = "uvx";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Arguments to pass to the command.";
        example = [ "mcp-server-fetch" ];
      };

      environment = mkOption {
        type = types.attrsOf types.str;
        default = { };
        description = "Environment variables specific to this named server.";
        example = {
          API_KEY = "secret";
        };
      };

      workingDirectory = mkOption {
        type = types.nullOr types.path;
        default = null;
        description = "Working directory for this named server process.";
      };
    };
  };

  # Generate command line arguments
  mkArgs =
    let
      mkServerScript =
        name: server:
        let
          app = pkgs.writeShellApplication {
            inherit name;
            text = "exec ${server.command} ${escapeShellArgs server.args}";
          };
        in
        "${app}/bin/${name}";
      # Server mode arguments
      serverArgs =
        optionals (cfg.defaultServer.enable) [ cfg.defaultServer.command ]
        ++ optionals (cfg.port != 0) [
          "--port"
          (toString cfg.port)
        ]
        ++ optionals (cfg.host != "127.0.0.1") [
          "--host"
          cfg.host
        ]
        ++ optionals cfg.stateless [ "--stateless" ]
        ++ optionals (cfg.allowOrigins != [ ]) (
          lib.concatMap (x: [
            "--allow-origin"
            x
          ]) cfg.allowOrigins
        )
        ++ flatten (
          mapAttrsToList (name: server: [
            "--named-server"
            name
            (mkServerScript name server)
          ]) (filterAttrs (_: server: server.enabled) cfg.namedServers)
        )
        ++ optionals (cfg.defaultServer.enable) [ "--" ]
        ++ cfg.defaultServer.args;

      # Global arguments
      globalArgs =
        optionals cfg.debug [ "--debug" ]
        ++ optionals cfg.passEnvironment [ "--pass-environment" ]
        ++ flatten (
          mapAttrsToList (k: v: [
            "--env"
            k
            v
          ]) cfg.environment
        )
        ++ optionals (cfg.workingDirectory != null) [
          "--cwd"
          cfg.workingDirectory
        ];

    in
    serverArgs ++ globalArgs;

  # Generate environment variables
  mkEnvironment =
    cfg.environment
    // (optionalAttrs cfg.passEnvironment (
      filterAttrs (n: _v: hasPrefix "MCP_" n || hasPrefix "UV_" n) config.environment.variables
    ));

in
{
  options.services.mcp-proxy = {
    enable = mkEnableOption "mcp-proxy service";

    package = mkOption {
      type = types.package;
      default = pkgs.mcp-proxy;
      defaultText = literalExpression "pkgs.mcp-proxy";
      description = "The mcp-proxy package to use.";
    };

    openFirewall = lib.mkOption {
      type = lib.types.bool;
      default = false;
      description = "Open firewall port for the service";
    };

    pkgs = lib.mkOption {
      description = "The package set";
      default = pkgs;
    };

    shell = lib.mkOption {
      type = lib.types.nullOr lib.types.package;
      default = null;
      description = "The devshell derivation to inject in agent's environment";
    };

    debug = mkOption {
      type = types.bool;
      default = false;
      description = "Enable debug mode with detailed logging output.";
    };

    passEnvironment = mkOption {
      type = types.bool;
      default = false;
      description = "Pass through all environment variables when spawning server processes.";
    };

    environment = mkOption {
      type = types.attrsOf types.str;
      default = { };
      description = "Environment variables for the mcp-proxy process and default server.";
      example = {
        HOME = "/var/lib/mcp-proxy";
      };
    };

    workingDirectory = mkOption {
      type = types.nullOr types.path;
      default = null;
      description = "Working directory for the default server process.";
    };

    user = mkOption {
      type = types.str;
      default = "mcp-proxy";
      description = "User to run the mcp-proxy service as.";
    };

    group = mkOption {
      type = types.str;
      default = "mcp-proxy";
      description = "Group to run the mcp-proxy service as.";
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = "Host address to bind the SSE server to.";
    };

    port = mkOption {
      type = types.port;
      default = 0;
      description = "Port to bind the SSE server to. 0 for random port.";
    };

    gid = mkOption {
      type = types.int;
      default = 1000;
      description = "GID for the mcp-proxy group. Ignored then group is overriden";
    };

    uid = mkOption {
      type = types.int;
      default = 1000;
      description = "UID for the mcp-proxy user. Ignored then user is overriden";
    };

    stateless = mkOption {
      type = types.bool;
      default = false;
      description = "Enable stateless mode for streamable HTTP transports.";
    };

    allowOrigins = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Allowed origins for CORS. Empty list disables CORS.";
      example = [
        "*"
        "https://example.com"
      ];
    };

    defaultServer = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable a default stdio server.";
      };

      command = mkOption {
        type = types.str;
        default = "";
        description = "Command to execute for the default stdio server.";
        example = "uvx";
      };

      args = mkOption {
        type = types.listOf types.str;
        default = [ ];
        description = "Arguments to pass to the default server command.";
        example = [ "mcp-server-fetch" ];
      };
    };

    namedServers = mkOption {
      type = types.attrsOf (types.submodule namedServerOptions);
      default = { };
      description = "Named stdio servers configuration.";
      example = literalExpression ''
        {
          fetch = {
            enabled = true;
            command = "uvx";
            args = [ "mcp-server-fetch" ];
          };
          github = {
            enabled = true;
            command = "npx";
            args = [ "-y" "@modelcontextprotocol/server-github" ];
            environment = { GITHUB_TOKEN = "ghp_..."; };
          };
        }
      '';
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.defaultServer.enable || cfg.namedServers != { };
        message = "services.mcp-proxy must have either defaultServer enabled or namedServers configured";
      }
      {
        assertion = cfg.defaultServer.enable -> cfg.defaultServer.command != "";
        message = "services.mcp-proxy.defaultServer.command must be set when defaultServer is enabled";
      }
    ];

    networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.port;

    users.users.${cfg.user} = mkIf (cfg.user == "mcp-proxy") {
      group = cfg.group;
      uid = cfg.uid;
      isNormalUser = true;
      description = "mcp-proxy service user";
      home = "/var/lib/mcp-proxy";
      createHome = true;
    };

    users.groups.${cfg.group} = mkIf (cfg.group == "mcp-proxy") {
      gid = cfg.gid;
    };

    systemd.services.mcp-proxy =
      let
        # Use the provided shell or create a minimal default
        shell =
          if cfg.shell != null then
            cfg.shell
          else
            pkgs.mkShell {
              buildInputs = with pkgs; [
                git
                curl
                bash
              ];
            };
        hookFile = pkgs.writeText "shellHook.source" (shell.shellHook or "");
        shellInputs = shell.buildInputs or [ ] ++ shell.nativeBuildInputs or [ ];
        startService = pkgs.writeShellApplication {
          name = "start";
          runtimeInputs =
            with pkgs;
            [
              git
              nix
              bash
              cfg.package
            ]
            ++ shellInputs;
          excludeShellChecks = [
            "SC1091" # So we can 'source' the shellHook
          ];
          text = ''
            source ${hookFile}
            mcp-proxy ${escapeShellArgs mkArgs};
          '';
        };
      in
      {
        description = "MCP Proxy Service";
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];

        serviceConfig = {
          Type = "exec";
          User = cfg.user;
          Group = cfg.group;
          ExecStart = "${startService}/bin/start";
          Restart = "always";
          RestartSec = "10s";

          # Security settings
          NoNewPrivileges = true;
          PrivateTmp = true;
          ProcSubset = "pid";
          ProtectProc = "invisible";
          ProtectSystem = "strict";
          ProtectHome = true;
          ProtectKernelTunables = true;
          ProtectKernelLogs = true;
          # Memory and Execution Protection
          MemoryDenyWriteExecute = true; # W^X enforcement
          LockPersonality = true; # Lock execution domain
          RestrictRealtime = true; # Block realtime scheduling
          RestrictSUIDSGID = true; # Block SUID/SGID execution
          ProtectKernelModules = true;
          ProtectControlGroups = true;
          RestrictAddressFamilies = [
            "AF_UNIX"
            "AF_INET"
            "AF_INET6"
          ];
          RestrictNamespaces = true;
          RemoveIPC = true;

          # Working directory
          WorkingDirectory = config.users.users."${cfg.user}".home;

          # Capabilities
          CapabilityBoundingSet = "";
          AmbientCapabilities = "";

          # File system access
          ReadWritePaths = [
            config.users.users."${cfg.user}".home
          ];
          ReadOnlyPaths = [
            "/etc/resolv.conf" # DNS resolution
            "/etc/hosts" # Host name resolution
            "/etc/nsswitch.conf" # Name service switch
            "/etc/ssl" # SSL certificates
            "/etc/ca-certificates" # CA certificates
          ];
          PrivateNetwork = false; # Ensure network access is available
        };

        environment = mkEnvironment;
      };

  };
}
