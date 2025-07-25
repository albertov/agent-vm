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

      runtimeInputs = mkOption {
        type = types.listOf types.package;
        default = with pkgs; [ ];
        description = "Runtime inputs";
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
          serverEnvVars = concatStringsSep "\n" (
            mapAttrsToList (k: v: ''export ${k}="${v}"'') server.environment
          );
          app = pkgs.writeShellApplication {
            inherit name;
            inherit (server) runtimeInputs;
            excludeShellChecks = [
              "SC1091" # So we can 'source' the shellHook
            ];
            text = ''
              source ${cfg.shellEnv}
              ${serverEnvVars}
              exec ${server.command} ${escapeShellArgs server.args}
            '';
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
          concatMap (x: [
            "--allow-origin"
            x
          ]) cfg.allowOrigins
        )
        ++ optionals (cfg.serverPrefix != "/servers") [
          "--server-prefix"
          cfg.serverPrefix
        ]
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

    openFirewall = mkOption {
      type = types.bool;
      default = false;
      description = "Open firewall port for the service";
    };

    pkgs = mkOption {
      description = "The package set";
      default = pkgs;
    };

    shellEnv = mkOption {
      type = types.path;
      description = ''
        The path to a file with the output of nix print-dev-env flake#shell
      '';
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
      description = "GID for the mcp-proxy group";
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

    serverPrefix = mkOption {
      type = types.str;
      default = "/servers";
      description = "URL prefix for named servers.";
      example = "/api/servers";
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
    runtimeInputs = mkOption {
      type = types.listOf types.package;
      default = with pkgs; [ ];
      description = "Runtime inputs common to all servers";
      example = [ "mcp-server-fetch" ];
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

    networking.firewall.allowedTCPPorts = optional cfg.openFirewall cfg.port;

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
        startService = pkgs.writeShellApplication {
          name = "start";
          runtimeInputs = [
            pkgs.bash
            cfg.package
          ] ++ cfg.runtimeInputs;
          excludeShellChecks = [
            "SC1091" # So we can 'source' the shellHook
          ];
          text = ''
            source ${cfg.shellEnv}
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

          # === CORE SECURITY ISOLATION ===

          # Prevent privilege escalation attacks - blocks setuid, capabilities acquisition
          NoNewPrivileges = true;

          # Provide isolated temporary directories (/tmp, /var/tmp) - prevents tmp race attacks
          PrivateTmp = true;

          # Hide other processes in /proc - equivalent to hidepid=2, improves process isolation
          ProcSubset = "pid"; # Only show PIDs belonging to this service
          ProtectProc = "invisible"; # Make other processes invisible in /proc

          # Filesystem protection - prevents tampering with system files
          ProtectSystem = "strict"; # Make entire root filesystem read-only except specific paths
          ProtectHome = true; # Block access to /home directories

          # === KERNEL PROTECTION ===

          # Block access to kernel configuration and sensitive system information
          ProtectKernelTunables = true; # Prevents writes to /proc/sys, /sys
          ProtectKernelLogs = true; # Blocks access to /proc/kmsg, /dev/kmsg kernel logs

          # === MEMORY AND EXECUTION PROTECTION ===
          # We need JIT compilers, etc..
          MemoryDenyWriteExecute = false;

          # Lock execution domain - prevents personality() syscall abuse
          LockPersonality = true; # Lock execution domain

          # Block realtime scheduling - prevents DoS via CPU monopolization
          RestrictRealtime = true; # Block realtime scheduling

          # Block SUID/SGID execution - prevents privilege escalation via setuid binaries
          RestrictSUIDSGID = true; # Block SUID/SGID execution

          # Prevents loading/unloading kernel modules, blocks /proc/kallsyms
          ProtectKernelModules = true;

          # === NAMESPACE AND IPC PROTECTION ===

          # Block control group modifications - prevents container escape via cgroup manipulation
          ProtectControlGroups = true;

          # Restrict network address families - only allow necessary network protocols
          RestrictAddressFamilies = [
            "AF_UNIX" # Unix domain sockets
            "AF_INET" # IPv4
            "AF_INET6" # IPv6
          ];

          # Block namespace creation - prevents unshare(), clone() with namespace flags
          RestrictNamespaces = true;

          # Remove IPC objects on service stop - prevents IPC persistence attacks
          RemoveIPC = true;

          # Working directory
          WorkingDirectory = "${config.users.users."${cfg.user}".home}/workspace";

          # === CAPABILITY RESTRICTION ===

          # Remove all Linux capabilities - service runs with minimal privileges
          CapabilityBoundingSet = ""; # No capabilities in bounding set
          AmbientCapabilities = ""; # No ambient capabilities inherited by children

          # === FILESYSTEM ACCESS CONTROL ===

          # Define exactly what paths the service can write to
          ReadWritePaths = [
            config.users.users."${cfg.user}".home # Service home directory only
          ];

          # Define essential read-only system paths needed for operation
          ReadOnlyPaths = [
            "/etc/resolv.conf" # DNS resolution configuration
            "/etc/hosts" # Host name resolution
            "/etc/nsswitch.conf" # Name service switch configuration
            "/etc/ssl" # SSL certificates directory
            "/etc/ca-certificates" # Certificate authority certificates
          ];

          # Network access (currently enabled for MCP proxy functionality)
          PrivateNetwork = false;
          # Ensure network access is available so the
          # agent can do dev work

          # === ADDITIONAL HARDENING RECOMMENDATIONS (COMMENTED OUT) ===
          # These options would provide even stronger security but may impact functionality
          # Uncomment and test carefully based on your specific requirements

          # # Resource limits - prevent resource exhaustion attacks
          # MemoryMax = "1G";                    # Limit maximum memory usage
          # TasksMax = 100;                      # Limit number of processes/threads
          # CPUQuota = "50%";                    # Limit CPU usage percentage

          # # System call filtering - block dangerous syscalls
          /*
            SystemCallFilter = [
              "@system-service" # Allow standard service syscalls
              #"~@debug" # Block debugging syscalls (ptrace, etc.)
              "~@mount" # Block mount operations
              "~@reboot" # Block reboot/shutdown syscalls
              "~@swap" # Block swap-related syscalls
              "~@privileged" # Block privileged operations
              "~@resources" # Block resource control syscalls
            ];
          */

          # # Additional process/system protection
          ProtectHostname = true; # Make hostname read-only
          ProtectClock = true; # Block system clock changes

          # # Device access restrictions
          PrivateDevices = true; # Provide minimal /dev with only essential devices
          DevicePolicy = "closed"; # Block access to all devices by default
          DeviceAllow = [
            # Explicitly allow only necessary devices
            "/dev/null rw" # Allow null device
            "/dev/zero rw" # Allow zero device
            "/dev/urandom r" # Allow random number generation
          ];

          # # Network isolation (if network not needed)
          # PrivateNetwork = true;               # Complete network isolation
          # IPAddressDeny = "any";               # Block all network access
          # IPAddressAllow = [ "localhost" ];    # Allow only localhost if needed

          # # More restrictive filesystem controls
          # ReadWritePaths = [                   # More restrictive write access
          #   "/var/lib/mcp-proxy/data"          # Only specific subdirectories
          #   "/var/lib/mcp-proxy/logs"          # Instead of entire home directory
          # ];
          # TemporaryFileSystem = [               # Override with tmpfs
          #   "/var:ro"                          # Make /var read-only with exceptions
          #   "/etc:ro"                          # Make /etc read-only
          # ];

          # # Advanced isolation (requires systemd 247+)
          # RootDirectory = "/var/lib/mcp-proxy"; # Chroot-like isolation
          # RootImage = "/path/to/image.raw";     # Use disk image as root

          # # Security monitoring and logging
          LogLevel = "debug"; # Detailed logging for security monitoring
          LogExtraFields = [
            # Additional log fields for analysis
            "USER"
            "UNIT"
            "INVOCATION_ID"
          ];
        };

        environment = mkEnvironment;
      };

  };
}
