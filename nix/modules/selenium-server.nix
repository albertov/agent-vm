{
  config,
  pkgs,
  lib,
  ...
}:
with lib;

let
  cfg = config.services.selenium-server;
  cfgFile = pkgs.writeText "config.json" (builtins.toJSON (cfg.settings));
in
{
  options = {
    services.selenium-server = {
      enable = mkEnableOption "Selenium server for system tests";
      package = mkOption {
        type = types.package;
        default = pkgs.selenium-server-standalone;
      };
      port = mkOption {
        type = types.int;
        default = 4444;
      };
      host = mkOption {
        type = types.str;
        default = "127.0.0.1";
      };
      openFirewall = mkOption {
        type = types.bool;
        default = false;
      };
      packages = mkOption {
        type = types.listOf types.package;
        default = with pkgs; [
          chromedriver
          chromium
          firefox
        ];
      };
      settings = mkOption {
        default = {
          inherit (cfg) host port;
          capabilities = [
            {
              browserName = "chrome";
              maxInstances = 10; # FIXME: make configurable
              seleniumProtocol = "WebDriver";
            }
          ];
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    users.groups.selenium-server = { };
    users.users.selenium-server = {
      description = "Selenium server owner";
      group = "selenium-server";
      home = "/var/lib/selenium-server";
      createHome = true;
      isSystemUser = true;
    };
    systemd.services.selenium-server = {
      description = "Selenium headless server";
      path = cfg.packages;
      wantedBy = [ "multi-user.target" ];
      after = [
        "network.target"
      ];
      postStart = ''
        while sleep 0.5; do
          if (: </dev/tcp/${cfg.settings.host}/${toString cfg.settings.port}) 2>/dev/null; then
            exit 0
          fi
        done
      '';
      serviceConfig = rec {
        Type = "exec";
        Restart = "always";
        ExecStart = "${cfg.package}/bin/selenium-server -config ${cfgFile}";
        User = "selenium-server";
        Group = "selenium-server";

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
        ProtectKernelModules = true; # Prevents loading/unloading kernel modules, blocks /proc/kallsyms

        # === MEMORY AND EXECUTION PROTECTION ===

        # Lock execution domain - prevents personality() syscall abuse
        LockPersonality = true; # Lock execution domain

        # Block realtime scheduling - prevents DoS via CPU monopolization
        RestrictRealtime = true; # Block realtime scheduling

        # Block SUID/SGID execution - prevents privilege escalation via setuid binaries
        RestrictSUIDSGID = true; # Block SUID/SGID execution

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
        WorkingDirectory = "/var/lib/selenium-server";

        # === CAPABILITY RESTRICTION ===

        # Remove all Linux capabilities - service runs with minimal privileges
        CapabilityBoundingSet = ""; # No capabilities in bounding set
        AmbientCapabilities = ""; # No ambient capabilities inherited by children

        # === FILESYSTEM ACCESS CONTROL ===

        # Define exactly what paths the service can write to
        ReadWritePaths = [
          "/var/lib/selenium-server" # Service home directory for logs and temporary files
        ];

        # Define essential read-only system paths needed for operation
        ReadOnlyPaths = [
          "/etc/resolv.conf" # DNS resolution configuration
          "/etc/hosts" # Host name resolution
          "/etc/nsswitch.conf" # Name service switch configuration
          "/etc/ssl" # SSL certificates directory
          "/etc/ca-certificates" # Certificate authority certificates
        ];

        # === DEVICE ACCESS RESTRICTIONS ===

        # Provide minimal /dev with only essential devices for browser operation
        PrivateDevices = true;
        DevicePolicy = "closed"; # Block access to all devices by default
        DeviceAllow = [
          # Explicitly allow only necessary devices for browser operation
          "/dev/null rw" # Allow null device
          "/dev/zero rw" # Allow zero device
          "/dev/urandom r" # Allow random number generation
          "/dev/shm rw" # Shared memory needed for browser rendering
          "/dev/dri rw" # Direct Rendering Infrastructure for GPU acceleration
        ];

        # === SYSTEM CALL FILTERING ===

        # Block dangerous syscalls while allowing necessary ones for browser operation
        SystemCallFilter = [
          "@system-service" # Allow standard service syscalls
          "~@debug" # Block debugging syscalls (ptrace, etc.)
          "~@mount" # Block mount operations
          "~@reboot" # Block reboot/shutdown syscalls
          "~@swap" # Block swap-related syscalls
          "~@privileged" # Block privileged operations
          "~@resources" # Block resource control syscalls
        ];

        # === ADDITIONAL HARDENING ===

        # Make hostname and system clock read-only
        ProtectHostname = true; # Make hostname read-only
        ProtectClock = true; # Block system clock changes

        # Network access (required for Selenium Grid and WebDriver functionality)
        PrivateNetwork = false; # Selenium needs network access for WebDriver protocol

        # Security monitoring and logging
        LogLevel = "info"; # Standard logging for security monitoring
        LogExtraFields = [
          # Additional log fields for security analysis
          "USER"
          "UNIT"
          "INVOCATION_ID"
        ];

        # === SELENIUM-SPECIFIC NOTES ===
        #
        # Note: MemoryDenyWriteExecute is NOT enabled because browsers (Chrome/Firefox)
        # require JIT compilation for JavaScript execution, which needs W+X memory pages.
        # This is a necessary security trade-off for browser automation functionality.
        #
        # Additional browser-related directories may need to be added to ReadWritePaths
        # if browsers require access to specific cache or configuration directories.
      };

    };
    networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.settings.port;
  };
}
