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
          geckodriver
          xvfb-run
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
        ExecStart = "${pkgs.xvfb-run}/bin/xvfb-run -a ${cfg.package}/bin/selenium-server -config ${cfgFile}";
        User = "selenium-server";
        Group = "selenium-server";

        # === CORE SECURITY ISOLATION ===

        # Prevent privilege escalation attacks - blocks setuid, capabilities acquisition
        # DISABLED: Chrome needs to set up its sandbox
        NoNewPrivileges = false;

        # Provide isolated temporary directories (/tmp, /var/tmp) - prevents tmp race attacks
        PrivateTmp = true; # Use private /tmp and /var/tmp

        # Hide other processes in /proc - equivalent to hidepid=2, improves process isolation
        # Relaxed for browsers which need to see other processes
        ProcSubset = "all"; # Show all PIDs (browsers need this for process management)
        ProtectProc = "default"; # Normal /proc visibility

        # Filesystem protection - prevents tampering with system files
        ProtectSystem = "full"; # Makes /usr, /boot, /efi read-only - browsers shouldn't write there
        ProtectHome = true; # Block access to /home directories

        # Additional bind mounts for browser functionality
        BindPaths = [
          "/sys/fs/cgroup" # Cgroup information for browser sandboxing
        ];

        # === KERNEL PROTECTION ===

        # Block access to kernel configuration and sensitive system information
        ProtectKernelTunables = true; # Prevents writes to /proc/sys, /sys - browsers shouldn't need this
        ProtectKernelLogs = true; # Blocks access to /proc/kmsg, /dev/kmsg kernel logs - browsers don't need kernel logs
        ProtectKernelModules = true; # Prevents loading/unloading kernel modules - browsers don't load kernel modules

        # === MEMORY AND EXECUTION PROTECTION ===

        # Lock execution domain - prevents personality() syscall abuse
        LockPersonality = true; # Browsers shouldn't need to change execution domain

        # Block realtime scheduling - prevents DoS via CPU monopolization
        RestrictRealtime = true; # Block realtime scheduling

        # Block SUID/SGID execution - prevents privilege escalation via setuid binaries
        RestrictSUIDSGID = true;

        # === NAMESPACE AND IPC PROTECTION ===

        # Block control group modifications - prevents container escape via cgroup manipulation
        ProtectControlGroups = true; # Browsers only need read access via bind mount

        # Restrict network address families - only allow necessary network protocols
        RestrictAddressFamilies = [
          "AF_UNIX" # Unix domain sockets
          "AF_INET" # IPv4
          "AF_INET6" # IPv6
        ];

        # Block namespace creation - prevents unshare(), clone() with namespace flags
        # Relaxed for browsers which need to create sandboxed child processes
        RestrictNamespaces = false;

        # Remove IPC objects on service stop - prevents IPC persistence attacks
        # DISABLED: Chrome needs IPC objects between its processes
        RemoveIPC = false;

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
          "/run/user" # Browser runtime directories
          "/dev/shm" # Chrome needs shared memory for rendering
        ];

        # Define essential read-only system paths needed for operation
        ReadOnlyPaths = [
          "/etc/resolv.conf" # DNS resolution configuration
          "/etc/hosts" # Host name resolution
          "/etc/nsswitch.conf" # Name service switch configuration
          "/etc/ssl" # SSL certificates directory
          "/nix/store"
          "/proc/sys/fs/binfmt_misc" # Binary format handlers
          "/run/current-system/sw" # System-wide software
          "/etc/fonts" # Font configuration
          "/etc/machine-id" # Machine ID (Chrome might check this)
          "/proc/sys/kernel/osrelease" # OS release info
          "/proc/sys/kernel/random/boot_id" # Boot ID
        ];

        # === DEVICE ACCESS RESTRICTIONS ===

        # Provide minimal /dev with only essential devices for browser operation
        PrivateDevices = true; # Create private /dev with minimal devices
        DevicePolicy = "closed"; # Block access to all devices by default
        DeviceAllow = [
          # Explicitly allow only necessary devices for browser operation
          "/dev/null rw" # Allow null device
          "/dev/zero rw" # Allow zero device
          "/dev/urandom r" # Allow random number generation
          "/dev/shm rw" # Shared memory needed for browser rendering
          "/dev/dri rw" # Direct Rendering Infrastructure for GPU acceleration
          "/dev/pts rw" # Pseudo terminals for process communication
          "/dev/ptmx rw" # PTY multiplexer for creating pseudo terminals
          "char-usb_device rwm" # USB devices (for some WebDriver features)
        ];

        # === SYSTEM CALL FILTERING ===

        # Block dangerous syscalls while allowing necessary ones for browser operation
        # DISABLED: Chrome aborts internally with any SystemCallFilter
        # Even with very permissive filters, Chrome crashes with SIGABRT
        # This appears to be Chrome detecting the seccomp filter and refusing to run
        # SystemCallFilter = [...];

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
        #
        # The service is wrapped with xvfb-run to provide a virtual framebuffer,
        # eliminating the need for a real display and handling DISPLAY variable automatically.
        #
        # Security settings have been relaxed in the following areas to allow browser drivers to function:
        # - PrivateTmp=false: Browsers need access to real /tmp directories
        # - RestrictNamespaces=false: Browsers create sandboxed child processes
        # - ProcSubset="all": Browsers need to see other processes for management
        # - Expanded ReadWritePaths: Added /tmp, /var/tmp, /run/user, /home/selenium-server for browser operations
        # - SystemCallFilter disabled: Chrome requires many syscalls (ptrace, pkey_alloc, etc.)
        # - Additional device permissions: Added PTY devices for process communication
        # - Added bind mount for /sys/fs/cgroup for browser sandboxing
        # - NoNewPrivileges=false: Chrome needs to set up its sandbox
        # - RemoveIPC=false: Chrome uses IPC between processes
        # - PrivateDevices=false: Chrome needs access to various devices
      };

      # Environment variables for browsers
      environment = {
        HOME = "/var/lib/selenium-server"; # Ensure HOME is set
      };

    };
    networking.firewall.allowedTCPPorts = lib.optional cfg.openFirewall cfg.settings.port;
  };
}
