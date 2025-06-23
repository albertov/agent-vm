# Agent Isolation Specification

## Overview

This document specifies the implementation of privilege-restricted agent execution using **NixOS QEMU VMs** for maximum security isolation. This approach uses hardware virtualization to provide significantly enhanced security boundaries while maintaining full functionality for code development and testing.

**Key Benefits of QEMU VM Approach:**
- **Complete Hardware Isolation**: Separate kernel instances with hypervisor security boundary
- **Memory Protection**: Hardware-assisted MMU protection prevents memory access between VMs
- **Network Virtualization**: Full network stack isolation with controlled host-guest communication
- **Filesystem Sharing**: High-performance VirtioFS for seamless workspace collaboration
- **Easy Shell Access**: SSH and console access methods for development workflows
- **Proven Reliability**: NixOS QEMU VM machinery is battle-tested and well-supported

## Integration Testing

### Integration Test Executable

The project includes a comprehensive integration test executable (`integration-test`) that validates the complete agent-vm workflow without using mocks. This provides end-to-end testing of:

- VM creation and configuration
- SSH key generation and authentication
- VM startup and agent service initialization
- MCP proxy functionality
- VM shutdown and cleanup
- Error handling and recovery scenarios

#### Features

- **CLI-only testing**: Calls `agent-vm` through command line exclusively (no mocks)
- **Isolated environments**: Uses temporary state directories for test isolation
- **Comprehensive coverage**: Tests complete workflows from creation to destruction
- **Colored output**: User-friendly test reporting with progress indicators
- **Nested virtualization aware**: Automatically detects and skips VM tests when virtualization is unavailable

#### Usage

```bash
# Run all integration tests
integration-test

# Run with verbose logging
integration-test --verbose

# Use specific agent-vm executable
integration-test --agent-vm ./path/to/agent-vm

# Run via nix flake
nix run .#integration-test

# Run with custom state directory (automatic in tests)
nix run .#integration-test -- --verbose
```

#### Test Structure

The integration test executable validates:

1. **VM Creation**: Configuration generation, SSH key creation, workspace cloning
2. **VM Listing**: Proper enumeration of created VMs
3. **VM Status**: Status reporting for stopped VMs
4. **VM Start/Stop Cycle**: Full VM lifecycle with agent service startup
5. **VM Destruction**: Complete cleanup and state removal

Tests automatically skip VM execution on systems without nested virtualization support while still validating configuration and CLI functionality.

#### CI/CD Integration

The integration test is designed for continuous integration environments:

- **Exit codes**: Returns 0 for success, 1 for failure, 130 for cancellation
- **Timeout handling**: Reasonable timeouts for VM operations
- **Resource cleanup**: Automatic cleanup even on test failures
- **Environment detection**: Graceful handling of virtualization constraints

## Host System Requirements

### Nested Virtualization Support

The agent VM system requires nested virtualization to run QEMU VMs within development environments that may themselves be virtualized. This enables running agent VMs inside cloud instances, development containers, or other virtualized environments.

#### NixOS Host Configuration

```nix
# /etc/nixos/configuration.nix (host system)
{ config, pkgs, ... }: {
  # Enable nested virtualization
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_amd nested=1
  '';

  # Enable required virtualization services
  virtualisation = {
    libvirtd.enable = true;
    qemu.package = pkgs.qemu_kvm;
  };

  users.users.agent-runner = {
    isNormalUser = true;
    extraGroups = [ "libvirtd" "kvm" ];
  };

  # Ensure KVM modules load
  boot.kernelModules = [ "kvm-intel" "kvm-amd" ];
}
```

#### Ubuntu/RedHat Host Configuration

For non-NixOS hosts, the agent-vm tool automatically detects the platform and provides setup guidance:

```bash
# Ubuntu/Debian setup (automated via agent-vm)
sudo apt update
sudo apt install qemu-kvm libvirt-daemon-system libvirt-clients bridge-utils

# RedHat/CentOS/Fedora setup (automated via agent-vm)
sudo dnf install qemu-kvm libvirt libvirt-daemon-config-network bridge-utils

# Common setup for all distributions
sudo usermod -aG libvirt,kvm $USER
echo 'options kvm_intel nested=1' | sudo tee /etc/modprobe.d/kvm.conf
echo 'options kvm_amd nested=1' | sudo tee -a /etc/modprobe.d/kvm.conf
sudo systemctl enable --now libvirtd
```

The agent-vm tool validates nested virtualization support and provides platform-specific setup instructions when needed.

## Architecture

### Core Components

1. **VM Configuration Management**: NixOS flake-based VM configurations with `virtualisation.vmVariant`
2. **Agent Service Module**: Configurable NixOS module for running agent as systemd service with minimal capabilities
3. **Workspace Sharing**: VirtioFS-based high-performance filesystem sharing for `/workspace`
4. **Network Isolation**: Port forwarding and network configuration for MCP server access
5. **VM Lifecycle Management**: Automated VM creation, startup, shell access, and cleanup
6. **Development Integration**: Seamless integration with existing MCP development workflow

### Current Agent System Analysis

The current agent system uses `mkMCPDevServers` which creates a shell application that:

1. **Sources development environment**: Applies shell hook from the `onping` devShell
2. **Provides MCP tools**: Includes codemcp, mcp-proxy, mcp-language-server, etc.
3. **Executes agent script**: Runs `./start.sh` which starts mcp-proxy with multiple servers:
   - `codemcp` - Core MCP functionality
   - `rescript-lsp` - ReScript language server via `./mcp_rescript_lsp.sh`
   - `mcp-nixos` - NixOS configuration tools
   - `selenium` - Web automation via `./mcp_selenium.sh`

**Target Architecture**: Replace the direct execution with VM-based isolation using systemd services while maintaining the same MCP server functionality and development experience. The agent will run as a systemd service with minimal necessary capabilities within the VM environment.

## Implementation Details

### 1. VM Configuration (vm-config.nix)

```nix
# ./vm-config.nix
{ config, pkgs, lib, ... }:
{
  # VM-specific configuration
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;        # 4GB RAM for development work
      cores = 4;                # 4 CPU cores
      diskSize = 20480;         # 20GB disk
      graphics = false;         # Headless for better performance

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
        { from = "host"; host.port = 8000; guest.port = 8000; }  # MCP proxy
        { from = "host"; host.port = 2222; guest.port = 22; }    # SSH access
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
    extraGroups = [ "wheel" ]; #TODO: Assess if necessary
    shell = pkgs.bash;
    # SSH key will be injected dynamically during VM startup
    openssh.authorizedKeys.keys = [
      # Ephemeral SSH public key will be added here during VM creation
    ];
    # Make the packages available to the agent available to the user too
    packages = config.services.agent-mcp.shell.buildInputs;
  };

  # SSH access for development with secure key-based authentication
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;  # Disable password auth for security
      PubkeyAuthentication = true;     # Enable key-based authentication
      PermitRootLogin = "no";
      AuthenticationMethods = "publickey";
    };
  };

  # Firewall configuration
  networking.firewall = {
    enable = true;
    allowedTCPPorts = [ 22 config.services.agent-mcp.port ];  # SSH and MCP proxy
  };

  # Optimize for VM environment
  services.qemuGuest.enable = true;

  # Security hardening
  security = {
    allowUserNamespaces = false;
    lockKernelModules = true;
    protectKernelImage = true;
  };

  # Import the agent service module
  imports = [ ./agent-service.nix ];

  # Enable and configure the agent service
  services.agent-mcp = {
    enable = true;
    user = "dev";
    group = "dev";
    workspaceDir = "/workspace";
    # These would be overrided in a module added by the create admin command
    # which imports this base config
    port = 8000;
    allowedOrigin = "https://claude.ai"
  };

  # System state version
  system.stateVersion = "24.11";
}
```

### 2. Agent Service Module (agent-service.nix)

```nix
# ./agent-service.nix
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
      type = lib.types.package;
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
      default = {};
      description = "Extra environment variables for the service";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.agent-mcp =
    let
       devServers = pkgs.mkMCPDevServers {
           inherit (cfg) name shell pkgs;
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
        StartLimitInterval = "60s";
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

    # Ensure the service user exists
    users.users = lib.mkIf (cfg.user != "root") {
      ${cfg.user} = {
        isSystemUser = true;
        group = cfg.group;
        description = "Agent MCP service user";
      };
    };

    users.groups = lib.mkIf (cfg.group != "root" && cfg.group != cfg.user) {
      ${cfg.group} = {};
    };
  };
}
```

### 3. VM Management Tool (agent-vm)

**Implementation Note**: The `agent-vm` tool is implemented as a typed Python application using `buildPythonApplication` and only uses Python stdlib modules for minimal dependencies and maximum compatibility.

#### CLI Features

The `agent-vm` command provides comprehensive VM lifecycle management with the following global options:

- `--verbose, -v`: Enable verbose logging with debug information
- `--state-dir STATE_DIR`: Override default state base directory (default: ~/.local/share/agent-vms)

The state directory override allows for:
- **Isolated testing environments**: Use custom directories for integration testing
- **Multiple development contexts**: Separate VM configurations per project or environment
- **CI/CD integration**: Ephemeral state directories for automated testing

#### Usage Examples

```bash
# Standard usage (uses ~/.local/share/agent-vms)
agent-vm create --branch=feature-x --port=8001
agent-vm start feature-x

# Custom state directory for isolated testing
agent-vm --state-dir /tmp/test-vms create --branch=test --port=8002
agent-vm --state-dir /tmp/test-vms start test

# Project-specific VM configurations
agent-vm --state-dir ./vms create --branch=main
```

```python
#!/usr/bin/env python3
"""
VM control command for managing development VMs.

This tool provides comprehensive VM lifecycle management including creation,
startup, shutdown, and workspace management for agent development environments.

Usage: agent-vm <command> [options]
"""

import argparse
import json
import logging
import os
import shutil
import subprocess
import sys
import time
from pathlib import Path
from typing import Dict, List, Optional, Tuple

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s [%(levelname)s] %(message)s",
    datefmt="%Y-%m-%d %H:%M:%S"
)
logger = logging.getLogger(__name__)


class VMController:
    """VM lifecycle management controller."""

    def __init__(self) -> None:
        """Initialize VM controller with configuration paths."""
        self.home_dir = Path.home()
        self.base_dir = self.home_dir / ".local" / "share" / "agent-vms"
        self.origin_repo = self._get_origin_repo()

    def _get_origin_repo(self) -> str:
        """Get the origin repository URL or path."""
        try:
            result = subprocess.run(
                ["git", "remote", "get-url", "origin"],
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()
        except subprocess.CalledProcessError:
            # Fallback to git root directory
            result = subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True,
                text=True,
                check=True
            )
            return result.stdout.strip()

    def _get_current_branch(self) -> str:
        """Get the current git branch name."""
        result = subprocess.run(
            ["git", "branch", "--show-current"],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()

    def create_vm(self, host: str = "localhost", port: int = 8000,
                  branch: Optional[str] = None, config: str = "./vm-config.nix") -> None:
        """Create a new VM configuration and workspace."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        workspace_dir = vm_config_dir / "workspace"

        if vm_config_dir.exists():
            logger.error(f"VM configuration already exists at: {vm_config_dir}")
            logger.info(f"Use 'agent-vm destroy {branch}' to remove it first")
            sys.exit(1)

        logger.info(f"Creating VM configuration directory: {vm_config_dir}")
        vm_config_dir.mkdir(parents=True, exist_ok=True)

        # Create SSH keypair for this VM
        ssh_dir = vm_config_dir / "ssh"
        ssh_dir.mkdir(mode=0o700, exist_ok=True)

        logger.info("Generating SSH keypair for VM access...")
        ssh_private_key = ssh_dir / "id_ed25519"
        ssh_public_key = ssh_dir / "id_ed25519.pub"

        subprocess.run([
            "ssh-keygen", "-t", "ed25519", "-f", str(ssh_private_key),
            "-N", "", "-C", f"vm-key-{branch}"
        ], capture_output=True, check=True)

        ssh_private_key.chmod(0o600)
        ssh_public_key.chmod(0o644)

        # Clone current repository at current branch
        logger.info("Cloning repository to workspace at current branch...")
        current_branch = self._get_current_branch()
        repo_root = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True, text=True, check=True
        ).stdout.strip()

        subprocess.run([
            "git", "clone", "--branch", current_branch, repo_root, str(workspace_dir)
        ], check=True)

        # Set up git remotes in workspace
        logger.info("Setting up git remotes...")
        os.chdir(workspace_dir)

        # Set origin to point to the source repository
        if self.origin_repo.startswith('/'):
            # Local path, use file:// scheme
            origin_url = f"file://{self.origin_repo}"
        else:
            # Remote URL, use as-is
            origin_url = self.origin_repo

        subprocess.run(["git", "remote", "set-url", "origin", origin_url], check=True)

        # Add upstream remote if different from origin
        try:
            os.chdir(repo_root)
            upstream_result = subprocess.run(
                ["git", "remote", "get-url", "origin"],
                capture_output=True, text=True
            )
            upstream_url = upstream_result.stdout.strip() if upstream_result.returncode == 0 else ""

            if upstream_url and upstream_url != self.origin_repo:
                os.chdir(workspace_dir)
                subprocess.run(["git", "remote", "add", "upstream", upstream_url])
        except subprocess.CalledProcessError:
            upstream_url = ""

        # Create VM config metadata
        logger.info("Creating VM configuration metadata...")
        config_data = {
            "branch": branch,
            "host": host,
            "port": port,
            "config_path": config,
            "workspace_path": str(workspace_dir),
            "ssh_key_path": str(ssh_private_key),
            "created_at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
            "origin_repo": self.origin_repo,
            "upstream_repo": upstream_url,
            "vm_name": f"agent-dev-{branch}"
        }

        config_file = vm_config_dir / "config.json"
        with config_file.open('w') as f:
            json.dump(config_data, f, indent=2)

        logger.info("VM configuration created successfully!")
        logger.info(f"Branch: {branch}")
        logger.info(f"Config directory: {vm_config_dir}")
        logger.info(f"Workspace: {workspace_dir}")
        logger.info(f"SSH key: {ssh_private_key}")
        logger.info("")
        logger.info(f"Start the VM with: agent-vm start {branch}")

    def start_vm(self, branch: Optional[str] = None) -> None:
        """Start VM from existing configuration."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            logger.info(f"Create one with: agent-vm create --branch={branch}")
            sys.exit(1)

        with config_file.open() as f:
            config_data = json.load(f)

        workspace_dir = Path(config_data["workspace_path"])
        ssh_key_path = Path(config_data["ssh_key_path"])
        vm_name = config_data["vm_name"]

        # Check if VM is already running
        if self._is_vm_running(vm_name):
            logger.warning("VM is already running")
            self._show_vm_status(vm_config_dir, config_data)
            return

        logger.info(f"Starting VM for branch: {branch}")
        os.chdir(workspace_dir)

        # Build VM configuration
        logger.info("Building VM configuration...")
        ssh_public_key = (ssh_key_path.parent / "id_ed25519.pub").read_text().strip()

        vm_build_cmd = [
            "nix", "build", "--no-link", "--print-out-paths",
            "--expr", f'''
            let
              pkgs = import <nixpkgs> {{}};
              vm = pkgs.nixos {{
                imports = [ {config_data["config_path"]} ];
                users.users.dev.openssh.authorizedKeys.keys = [ "{ssh_public_key}" ];
                virtualisation.vmVariant.virtualisation.sharedDirectories.workspace.source = "{workspace_dir}";
              }};
            in vm.config.system.build.vm'''
        ]

        result = subprocess.run(vm_build_cmd, capture_output=True, text=True, check=True)
        vm_path = result.stdout.strip()

        if not vm_path:
            logger.error("Failed to build VM")
            sys.exit(1)

        # Start VM in background
        logger.info("Starting VM...")
        vm_cmd = [f"{vm_path}/bin/run-nixos-vm"]

        env = os.environ.copy()
        env["QEMU_OPTS"] = f"-name {vm_name}"

        vm_process = subprocess.Popen(vm_cmd, env=env)

        # Store PID
        pid_file = vm_config_dir / "vm.pid"
        with pid_file.open('w') as f:
            f.write(str(vm_process.pid))

        # Wait for VM to be ready
        if self._wait_for_vm_ready(ssh_key_path):
            logger.info(f"VM started successfully (PID: {vm_process.pid})")

            # Start agent services in VM
            self._start_agent_in_vm(ssh_key_path)
        else:
            logger.error("VM startup failed")
            self._stop_vm_by_pid(vm_config_dir)
            sys.exit(1)

    def stop_vm(self, branch: Optional[str] = None) -> None:
        """Stop VM."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            sys.exit(1)

        logger.info(f"Stopping VM for branch: {branch}")
        self._stop_vm_by_pid(vm_config_dir)

    def vm_status(self, branch: Optional[str] = None) -> None:
        """Get VM status."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.info(f"No VM configuration found for branch: {branch}")
            return

        with config_file.open() as f:
            config_data = json.load(f)

        logger.info(f"VM status for branch: {branch}")
        self._show_vm_status(vm_config_dir, config_data)

    def vm_shell(self, branch: Optional[str] = None) -> None:
        """Open shell in VM."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            sys.exit(1)

        with config_file.open() as f:
            config_data = json.load(f)

        vm_name = config_data["vm_name"]
        ssh_key_path = Path(config_data["ssh_key_path"])

        if not self._is_vm_running(vm_name):
            logger.error("VM is not running. Start it first with: agent-vm start")
            sys.exit(1)

        logger.info(f"Opening shell in VM for branch: {branch}")

        # SSH into VM
        ssh_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", "2222", "dev@localhost"
        ]

        subprocess.run(ssh_cmd)

    def list_vms(self) -> None:
        """List all VM configurations."""
        if not self.base_dir.exists():
            logger.info("No VM configurations found")
            return

        logger.info("Available VM configurations:")
        for config_dir in self.base_dir.iterdir():
            if config_dir.is_dir():
                config_file = config_dir / "config.json"
                if config_file.exists():
                    try:
                        with config_file.open() as f:
                            config_data = json.load(f)
                        created_at = config_data.get("created_at", "unknown")
                        logger.info(f"  {config_dir.name} (created: {created_at})")
                    except (json.JSONDecodeError, OSError):
                        logger.info(f"  {config_dir.name} (invalid config)")

    def destroy_vm(self, branch: Optional[str] = None) -> None:
        """Destroy VM configuration."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch

        if not vm_config_dir.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            sys.exit(1)

        # Stop VM if running
        config_file = vm_config_dir / "config.json"
        if config_file.exists():
            try:
                self.stop_vm(branch)
            except subprocess.CalledProcessError:
                pass  # Ignore if already stopped

        logger.info(f"Destroying VM configuration for branch: {branch}")
        shutil.rmtree(vm_config_dir)
        logger.info(f"VM configuration destroyed: {vm_config_dir}")

    def vm_logs(self, branch: Optional[str] = None) -> None:
        """Show VM logs."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            sys.exit(1)

        with config_file.open() as f:
            config_data = json.load(f)

        vm_name = config_data["vm_name"]
        ssh_key_path = Path(config_data["ssh_key_path"])

        logger.info(f"Showing logs for VM: {branch}")

        if self._is_vm_running(vm_name):
            logger.info("VM is running. Checking agent service logs via SSH...")
            try:
                # Check agent service logs in VM
                ssh_cmd = [
                    "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", "2222", "dev@localhost",
                    "journalctl -f --no-pager -n 50 -u agent-mcp"
                ]
                subprocess.run(ssh_cmd)
            except subprocess.CalledProcessError:
                logger.info("Could not access agent service logs. SSH into VM to check manually:")
                logger.info(f"agent-vm shell {branch}")
                logger.info("Then run: journalctl -f -u agent-mcp")
        else:
            logger.info("VM is not running")

    def _is_vm_running(self, vm_name: str) -> bool:
        """Check if VM is running by process name."""
        try:
            subprocess.run(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True, check=True
            )
            return True
        except subprocess.CalledProcessError:
            return False

    def _wait_for_vm_ready(self, ssh_key_path: Path, max_attempts: int = 30) -> bool:
        """Wait for VM to be ready for SSH connections."""
        logger.info("Waiting for VM to be ready...")

        for attempt in range(max_attempts):
            try:
                ssh_cmd = [
                    "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", "2222", "dev@localhost", "echo 'VM Ready'"
                ]
                result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
                if result.returncode == 0:
                    logger.info("VM is ready for connections")
                    return True
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                pass

            print(".", end="", flush=True)
            time.sleep(2)

        print()  # New line after dots
        logger.error(f"VM failed to become ready after {max_attempts} attempts")
        return False

    def _start_agent_in_vm(self, ssh_key_path: Path) -> None:
        """Start agent services in VM using systemd."""
        logger.info("Starting agent services in VM...")

        ssh_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", "2222", "dev@localhost",
            "sudo systemctl start agent-mcp"
        ]

        try:
            subprocess.run(ssh_cmd, check=True)
            logger.info("Agent services started in VM")

            # Wait for service to be ready
            self._wait_for_agent_ready(ssh_key_path)
            logger.info("MCP Proxy available at: http://localhost:8000")
            logger.info("To access VM shell: agent-vm shell")
        except subprocess.CalledProcessError:
            logger.error("Failed to start agent services in VM")

    def _wait_for_agent_ready(self, ssh_key_path: Path, max_attempts: int = 20) -> bool:
        """Wait for agent service to be ready."""
        logger.info("Waiting for agent service to be ready...")

        for attempt in range(max_attempts):
            try:
                ssh_cmd = [
                    "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", "2222", "dev@localhost",
                    "curl -f http://localhost:8000/health"
                ]
                result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
                if result.returncode == 0:
                    logger.info("Agent service is ready")
                    return True
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                pass

            print(".", end="", flush=True)
            time.sleep(2)

        print()  # New line after dots
        logger.error(f"Agent service failed to become ready after {max_attempts} attempts")
        return False

    def _stop_vm_by_pid(self, vm_config_dir: Path) -> None:
        """Stop VM using stored PID file."""
        pid_file = vm_config_dir / "vm.pid"

        if pid_file.exists():
            try:
                pid = int(pid_file.read_text().strip())
                logger.info(f"Stopping VM (PID: {pid})...")

                # Try graceful shutdown first
                os.kill(pid, 15)  # SIGTERM

                # Wait a bit for graceful shutdown
                for _ in range(5):
                    try:
                        os.kill(pid, 0)  # Check if process exists
                        time.sleep(1)
                    except ProcessLookupError:
                        break
                else:
                    # Force kill if still running
                    logger.warning("VM didn't stop gracefully, forcing shutdown...")
                    try:
                        os.kill(pid, 9)  # SIGKILL
                    except ProcessLookupError:
                        pass

            except (ValueError, ProcessLookupError):
                pass
            finally:
                pid_file.unlink(missing_ok=True)

        # Also cleanup any remaining QEMU processes by name
        try:
            with (vm_config_dir / "config.json").open() as f:
                config_data = json.load(f)
            vm_name = config_data["vm_name"]
            subprocess.run(["pkill", "-f", f"qemu.*{vm_name}"], capture_output=True)
        except (FileNotFoundError, json.JSONDecodeError, subprocess.CalledProcessError):
            pass

        logger.info("VM stopped")

    def _show_vm_status(self, vm_config_dir: Path, config_data: Dict) -> None:
        """Show detailed VM status."""
        vm_name = config_data["vm_name"]
        ssh_key_path = Path(config_data["ssh_key_path"])

        if self._is_vm_running(vm_name):
            logger.info("VM Status: Running")
            logger.info("SSH Access: ssh -p 2222 dev@localhost")
            logger.info("MCP Proxy: http://localhost:8000 (if agent is running)")

            # Check if agent is running in VM
            try:
                ssh_cmd = [
                    "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", "2222", "dev@localhost", "systemctl is-active agent-mcp"
                ]
                result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
                if result.returncode == 0 and result.stdout.decode().strip() == "active":
                    logger.info("Agent Status: Running")
                else:
                    logger.warning("Agent Status: Not running")
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                logger.warning("Agent Status: Could not check")
        else:
            logger.info("VM Status: Stopped")


def main() -> None:
    """Main entry point for agent-vm command."""
    parser = argparse.ArgumentParser(
        description="VM control command for managing development VMs",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  agent-vm create --branch=feature-x --port=8001
  agent-vm start feature-x
  agent-vm shell feature-x
  agent-vm list
  agent-vm destroy feature-x
        """
    )

    subparsers = parser.add_subparsers(dest='command', help='Available commands')

    # Create command
    create_parser = subparsers.add_parser('create', help='Create a new VM configuration')
    create_parser.add_argument('--host', default='localhost', help='Host to bind VM ports to')
    create_parser.add_argument('--port', type=int, default=8000, help='Port for MCP proxy forwarding')
    create_parser.add_argument('--branch', help='Branch name for VM (default: current branch)')
    create_parser.add_argument('--config', default='./vm-config.nix', help='Path to VM NixOS config')

    # Start command
    start_parser = subparsers.add_parser('start', help='Start VM for branch')
    start_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Stop command
    stop_parser = subparsers.add_parser('stop', help='Stop VM for branch')
    stop_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Status command
    status_parser = subparsers.add_parser('status', help='Show VM status for branch')
    status_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Shell command
    shell_parser = subparsers.add_parser('shell', help='Open SSH shell in VM for branch')
    shell_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Logs command
    logs_parser = subparsers.add_parser('logs', help='Show VM logs for branch')
    logs_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # List command
    subparsers.add_parser('list', help='List all VM configurations')

    # Destroy command
    destroy_parser = subparsers.add_parser('destroy', help='Destroy VM configuration for branch')
    destroy_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    args = parser.parse_args()

    if not args.command:
        parser.print_help()
        sys.exit(1)

    controller = VMController()

    try:
        if args.command == 'create':
            controller.create_vm(args.host, args.port, args.branch, args.config)
        elif args.command == 'start':
            controller.start_vm(args.branch)
        elif args.command == 'stop':
            controller.stop_vm(args.branch)
        elif args.command == 'status':
            controller.vm_status(args.branch)
        elif args.command == 'shell':
            controller.vm_shell(args.branch)
        elif args.command == 'logs':
            controller.vm_logs(args.branch)
        elif args.command == 'list':
            controller.list_vms()
        elif args.command == 'destroy':
            controller.destroy_vm(args.branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        sys.exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        sys.exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
```

### 3. Updated MCP Dev Servers Integration

```nix
# Update to ./overlay.nix
inputs: final: prev:
{
  mkMCPDevServers = { name ? "start-agent", shell, pkgs ? final, useVM ? true }:
    let
      hookFile = pkgs.writeText "shellHook.source" (shell.shellHook or "");
      shellInputs =
        shell.buildInputs or [ ]
        ++ shell.nativeBuildInputs or [ ];

      # All MCP tools that need to be available
      mcpTools = with pkgs; [
        git nix codemcp mcp-proxy mcp-language-server
        rescript-language-server
        inputs.mcp-selenium.packages.${pkgs.system}.default
        inputs.mcp-nixos.packages.${pkgs.system}.default
      ];

      # Python VM management tool
      agent-vm = pkgs.python3.pkgs.buildPythonApplication {
        pname = "agent-vm";
        version = "1.0.0";
        src = ./agent-vm.py;
        format = "other";

        # Only Python stdlib - no dependencies
        propagatedBuildInputs = [ ];

        installPhase = ''
          mkdir -p $out/bin
          cp $src $out/bin/agent-vm
          chmod +x $out/bin/agent-vm
        '';

        # Type checking during build
        checkPhase = ''
          ${pkgs.python3}/bin/python -m py_compile $src
        '';

        meta = {
          description = "VM control tool for managing development VMs";
          license = pkgs.lib.licenses.mit;
        };
      };

      # VM-based agent launcher
      vmAgentScript = pkgs.writeShellApplication {
        name = "start-vm-agent";
        runtimeInputs = [
          pkgs.openssh pkgs.qemu agent-vm
        ] ++ shellInputs;  # Include shell deps for VM building
        excludeShellChecks = [ "SC1091" "SC2034" ];
        text = ''
          # Source shell hook for environment setup (needed for VM building)
          source ${hookFile}

          # Use Python VM controller directly
          exec agent-vm "$@"
        '';
      };

      # Direct agent launcher (current behavior)
      directAgentScript = pkgs.writeShellApplication {
        inherit name;
        runtimeInputs = mcpTools ++ shellInputs;
        excludeShellChecks = [ "SC1091" ];
        text = ''
          source ${hookFile}
          exec ${./start.sh} "$@"
        '';
      };

    in
    if useVM then vmAgentScript else directAgentScript;

  # ... rest of overlay unchanged
}
```

### 4. Flake Integration

```nix
# Update to flake.nix apps section
apps = flakeNative.apps // rec {
  # ... existing apps ...

  # VM-based agent (new default)
  agent = inputs.flake-utils.lib.mkApp {
    drv = pkgs.mkMCPDevServers {
      name = "mcp-agents-vm";
      shell = self.outputs.devShells.${system}.onping;
      pkgs = pkgs.nixpkgs-unstable;
      useVM = true;
    };
  };

  # Direct agent (fallback)
  agent-direct = inputs.flake-utils.lib.mkApp {
    drv = pkgs.mkMCPDevServers {
      name = "mcp-agents-direct";
      shell = self.outputs.devShells.${system}.onping;
      pkgs = pkgs.nixpkgs-unstable;
      useVM = false;
    };
  };

  # Default points to VM version
  default = agent;
};
```

## Security Analysis

### Hardware Virtualization Security Benefits

**VM Isolation vs Container Isolation:**

| Security Aspect | systemd-nspawn | QEMU VMs |
|-----------------|----------------|----------|
| **Kernel Isolation** | Shared kernel | Separate kernel instances |
| **Memory Protection** | Process isolation | Hardware MMU protection |
| **Attack Surface** | Large (shared kernel) | Minimal (hypervisor only) |
| **Escape Difficulty** | Medium (namespace escapes) | Very High (VM escapes rare) |
| **Network Isolation** | Namespace-based | Full network stack isolation |
| **Resource Controls** | cgroups | Hardware-enforced limits |

**Security Threat Model:**

**Protected Against:**
1. **Kernel Exploits**: VM uses separate kernel, host kernel exploits don't affect guest
2. **Memory Attacks**: Hardware MMU prevents direct memory access between host/guest
3. **Network Attacks**: Complete network stack isolation with controlled communication
4. **Filesystem Attacks**: VirtioFS provides secure filesystem sharing with controlled access
5. **Resource Exhaustion**: Hardware-enforced CPU/memory limits prevent DoS
6. **Process Interference**: Complete process space isolation
7. **Device Access**: No direct hardware access, all through virtualized interfaces

**VM-Specific Security:**
- **Hypervisor Security**: QEMU runs unprivileged with additional sandboxing
- **SELinux/AppArmor**: Additional mandatory access controls on QEMU processes
- **Seccomp Filtering**: System call filtering for QEMU process
- **VM Escape Prevention**: Hardware-assisted virtualization makes escapes extremely difficult

### Resource Overhead Analysis

**Performance Comparison:**

| Metric | systemd-nspawn | QEMU VM | Overhead Factor |
|--------|----------------|---------|-----------------|
| **Memory** | 5-15MB | 256-512MB | 15-35x |
| **Startup Time** | 1-3 seconds | 15-45 seconds | 15x |
| **CPU Overhead** | 0-5% | 5-15% | 3x |
| **I/O Performance** | Native | VirtioFS ~85% | 1.2x |
| **Network Latency** | Native | +1-2ms | Minimal |

**When to Use VMs:**
- ✅ **High-security requirements**: Untrusted code execution
- ✅ **Compliance needs**: Regulatory isolation requirements
- ✅ **Development isolation**: Multiple agent instances
- ✅ **Testing environments**: Different OS/kernel configurations
- ❌ **Resource-constrained**: Limited RAM/CPU environments
- ❌ **Rapid iteration**: Very frequent start/stop cycles

## Usage Examples

### Basic VM Setup and Management

```bash
# Create a new VM configuration for current branch
agent-vm create

# Create VM configuration for specific branch with custom port
agent-vm create --branch=feature-authentication --port=8001

# Start the VM (will use existing configuration)
agent-vm start feature-authentication

# Check VM status
agent-vm status feature-authentication

# Open shell in the VM for debugging
agent-vm shell feature-authentication

# Stop the VM
agent-vm stop feature-authentication

# List all VM configurations
agent-vm list

# Destroy VM configuration when done
agent-vm destroy feature-authentication
```

### Legacy VM Usage (without agent-vm)

```bash
# Start VM-based agent (creates temporary workspace)
nix run .#agent-vm

# VM management commands for existing workflow
nix run .#agent-vm -- create   # Create a new VM state dir
nix run .#agent-vm -- destroy  # Destory an existing state dir
nix run .#agent-vm -- start    # Start existing VM with agent
nix run .#agent-vm -- stop     # Stop existing VM
nix run .#agent-vm -- status   # Show existing VM status
nix run .#agent-vm -- logs     # Show existing VM logs for a service using journalctl (forward args)
nix run .#agent-vm -- shell    # Open existing shell in VM
nix run .#agent-vm -- restart  # Restart existing VM and agent

# Fallback to direct execution
nix run .#agent
```

### Development Workflow

```bash
# Set up development environment for a new feature
agent-vm create --branch=feature-new-auth

# Start the VM and development environment
agent-vm start feature-new-auth

# In another terminal, access VM for debugging
agent-vm shell feature-new-auth

# Inside VM, check agent services
sudo systemctl status agent-mcp      # Check agent service status
journalctl -f -u agent-mcp           # View service logs
sudo systemctl restart agent-mcp     # Restart service if needed

# Access workspace files (shared via VirtioFS)
# Changes made in ~/.local/share/agent-vms/feature-new-auth/workspace
# are immediately visible in the VM at /workspace
ls /workspace  # Same content as host workspace

# When done with development, clean up
agent-vm stop feature-new-auth
agent-vm destroy feature-new-auth
```

### Multi-Instance Development

```bash
# Create and start multiple isolated agent instances for different features
agent-vm create --branch=feature-auth --port=8001
agent-vm create --branch=feature-ui --port=8002
agent-vm create --branch=bugfix-login --port=8003

# Start all VMs
agent-vm start feature-auth &
agent-vm start feature-ui &
agent-vm start bugfix-login &

# Each VM gets its own:
# - Isolated workspace at ~/.local/share/agent-vms/<branch>/workspace
# - Unique SSH keypair for secure access
# - Separate port for MCP proxy (8001, 8002, 8003)
# - Independent git remotes and branch configuration
# - Complete environment isolation

# Check status of all VMs
agent-vm list

# Work with specific feature
agent-vm shell feature-auth
```

## Implementation Timeline

### Phase 1: Core VM Infrastructure ✏️
- [ ] Create `./vm-config.nix` with basic NixOS VM configuration
- [ ] Implement `./start-vm-agent.sh` with VM lifecycle management
- [ ] Implement `./agent-vm` VM management tool with create/start/stop/destroy commands
- [ ] Update `./overlay.nix` to support VM-based execution
- [ ] Add VM configuration to flake apps

### Phase 2: Workspace Integration ✏️
- [ ] Implement VirtioFS workspace sharing with persistent directories
- [ ] Add persistent SSH keypair management in VM config directories
- [ ] Configure git remotes setup for origin and upstream repositories
- [ ] Configure SSH access and port forwarding
- [ ] Test MCP server functionality in VM environment

### Phase 3: Development Experience ✏️
- [ ] Add VM status monitoring and health checks
- [ ] Implement graceful VM shutdown and cleanup
- [ ] Add shell access shortcuts and debugging tools
- [ ] Create documentation and usage examples

### Phase 4: Security Hardening ✏️
- [ ] Implement VM security configurations
- [ ] Add network isolation and firewall rules
- [ ] Configure resource limits and monitoring
- [ ] Security audit and penetration testing

### Phase 5: Production Readiness ✏️
- [ ] Performance optimization and tuning
- [ ] Error handling and recovery mechanisms
- [ ] Multi-instance support and orchestration
- [ ] CI/CD integration and automated testing

## Migration Strategy

**Gradual Migration Approach:**

1. **Parallel Implementation**: Keep both VM and direct execution options
2. **Default to VM**: Make VM execution the default while maintaining fallback
3. **Testing Period**: Extensive testing with VM approach
4. **Sunset systemd-nspawn**: Remove old container-based code after validation

**Compatibility Considerations:**

- **Same Interface**: `nix run .#agent` continues to work with new VM backend
- **Environment Parity**: VM contains same tools and configurations as direct execution
- **Workspace Access**: Files remain accessible in same location (`/workspace`)
- **MCP Compatibility**: All existing MCP servers work identically in VM environment

## Summary of Key Changes

This specification has been updated to implement persistent VM configurations and enhanced workspace management:

### SSH Key Management Updates
- **Persistent SSH Keys**: SSH keypairs are now created once during `agent-vm create` and stored in `~/.local/share/mcp-./<branch>/ssh/`
- **No More Ephemeral Keys**: Eliminated temporary SSH key creation and cleanup on each VM start/stop
- **Secure Storage**: SSH keys are stored with proper permissions (600 for private, 644 for public) in the VM's config directory

### New VM Management Tool
- **`agent-vm` Command**: New comprehensive VM management tool with create, start, stop, status, shell, list, and destroy commands
- **Branch-Based VMs**: Each branch gets its own isolated VM configuration and workspace
- **Persistent Configuration**: VM settings stored in JSON metadata files for easy management

### Workspace Management
- **Persistent Workspaces**: Each VM has a dedicated workspace at `~/.local/share/mcp-./<branch>/workspace/`
- **Git Remote Setup**: Automatic configuration of origin (with file:// scheme) and upstream remotes
- **Repository Cloning**: Full repository clone during VM creation instead of simple copying

### Enhanced Multi-Instance Support
- **Branch Isolation**: Multiple VMs can run simultaneously for different branches
- **Port Management**: Each VM can use different ports for MCP proxy forwarding
- **Independent Configuration**: Each VM maintains its own SSH keys, workspace, and configuration

The updated approach provides better isolation, persistence, and management capabilities while maintaining the same security benefits of hardware virtualization.

## Conclusion

The transition to QEMU VMs provides significantly enhanced security isolation for agent execution while maintaining the same development experience. Hardware virtualization creates strong security boundaries that are essential for running potentially untrusted agent code. The implementation leverages NixOS's excellent VM support and provides a solid foundation for secure agent development.

**Key Benefits Achieved:**
- **Maximum Security**: Hardware-level isolation with separate kernels
- **Development Continuity**: Same tools, same workflow, same MCP servers
- **Operational Simplicity**: Single command to start/stop/manage VM environments
- **Future-Proof**: Foundation for advanced security features and multi-tenancy

The approach balances security requirements with development productivity, creating a robust platform for secure agent development and testing.
