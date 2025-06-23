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
    datefmt="%Y-%m-%d %H:%M:%S",
    stream=sys.stdout  # Ensure output goes to stdout
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

    def _cleanup_stale_processes(self, vm_name: str) -> None:
        """Clean up any stale VM processes."""
        try:
            # Find stale QEMU processes
            result = subprocess.run(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True, text=True
            )
            if result.returncode == 0:
                pids = result.stdout.strip().split('\n')
                for pid in pids:
                    if pid.strip():
                        try:
                            logger.warning(f"Cleaning up stale VM process: {pid}")
                            os.kill(int(pid), 15)  # SIGTERM
                            time.sleep(1)
                            os.kill(int(pid), 9)   # SIGKILL if still alive
                        except (ProcessLookupError, ValueError):
                            pass
        except subprocess.CalledProcessError:
            pass  # No stale processes found

    def _get_current_branch(self) -> str:
        """Get the current git branch name."""
        try:
            result = subprocess.run(
                ["git", "branch", "--show-current"],
                capture_output=True,
                text=True,
                check=True
            )
            branch = result.stdout.strip()
            if not branch:
                # Detached HEAD state, use commit hash
                result = subprocess.run(
                    ["git", "rev-parse", "--short", "HEAD"],
                    capture_output=True,
                    text=True,
                    check=True
                )
                branch = f"detached-{result.stdout.strip()}"
            return branch
        except subprocess.CalledProcessError:
            # Not in a git repository or git not available
            logger.warning("Not in a git repository or git not available, using 'default' as branch name")
            return "default"

    def create_vm(self, host: str = "localhost", port: int = 8000,
                  branch: Optional[str] = None, config: str = "vm-config.nix") -> None:
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

        try:
            repo_root = subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True, text=True, check=True
            ).stdout.strip()

            subprocess.run([
                "git", "clone", "--branch", current_branch, repo_root, str(workspace_dir)
            ], check=True)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to clone repository: {e}")
            # Clean up partially created VM config
            if vm_config_dir.exists():
                shutil.rmtree(vm_config_dir)
            sys.exit(1)

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

        # Clean up any stale processes first
        self._cleanup_stale_processes(vm_name)

        logger.info(f"Starting VM for branch: {branch}")

        # Get the original flake directory before changing to workspace
        try:
            original_flake_dir = subprocess.run(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True, text=True, check=True
            ).stdout.strip()
        except subprocess.CalledProcessError:
            logger.error("Could not determine original flake directory")
            sys.exit(1)

        os.chdir(workspace_dir)

        # Build VM configuration
        logger.info("Building VM configuration...")
        ssh_public_key = (ssh_key_path.parent / "id_ed25519.pub").read_text().strip()

        # Create a temporary VM configuration file with injected SSH key
        temp_config_content = f'''
# Temporary VM config with injected SSH key
{{ config, pkgs, lib, ... }}:
{{
  imports = [ {original_flake_dir}/{config_data["config_path"]} ];

  users.users.dev.openssh.authorizedKeys.keys = [ "{ssh_public_key}" ];

  virtualisation.vmVariant.virtualisation.sharedDirectories.workspace.source = lib.mkForce "{workspace_dir}";

  # Override port configuration
  services.agent-mcp.port = lib.mkForce {config_data["port"]};

  # Override port forwarding
  virtualisation.vmVariant.virtualisation.forwardPorts = lib.mkForce [
    {{ from = "host"; host.port = {config_data["port"]}; guest.port = {config_data["port"]}; }}
    {{ from = "host"; host.port = 2222; guest.port = 22; }}
  ];

  # Override firewall ports
  networking.firewall.allowedTCPPorts = lib.mkForce [ 22 {config_data["port"]} ];
}}
'''

        temp_config_path = Path("temp-vm-config.nix")
        with temp_config_path.open('w') as f:
            f.write(temp_config_content)

        try:
            vm_build_cmd = [
                "nix", "build", "--no-link", "--print-out-paths", "--impure",
                "--expr", f'''
                let
                  flake = builtins.getFlake "{original_flake_dir}";
                  pkgs = flake.legacyPackages.${{builtins.currentSystem}};
                  vm = pkgs.nixos {{
                    imports = [ ./temp-vm-config.nix ];
                  }};
                in vm.config.system.build.vm'''
            ]

            result = subprocess.run(vm_build_cmd, capture_output=True, text=True, cwd=workspace_dir)
            if result.returncode != 0:
                logger.error(f"VM build failed: {result.stderr}")
                logger.error(f"VM build stdout: {result.stdout}")
                raise subprocess.CalledProcessError(result.returncode, vm_build_cmd)
            vm_path = result.stdout.strip()
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to build VM configuration: {e}")
            sys.exit(1)
        finally:
            # Clean up temporary config file
            temp_config_path.unlink(missing_ok=True)

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

    def restart_vm(self, branch: Optional[str] = None) -> None:
        """Restart VM (stop then start)."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            logger.info(f"Create one with: agent-vm create --branch={branch}")
            sys.exit(1)

        logger.info(f"Restarting VM for branch: {branch}")

        # Stop VM if running
        try:
            self.stop_vm(branch)
        except subprocess.CalledProcessError:
            logger.warning("VM was already stopped or failed to stop cleanly")

        # Wait a moment for cleanup
        time.sleep(2)

        # Start VM
        self.start_vm(branch)

    def vm_status(self, branch: Optional[str] = None) -> None:
        """Get VM status."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch
        config_file = vm_config_dir / "config.json"

        if not config_file.exists():
            logger.info(f"No VM configuration found for branch: {branch}")
            logger.info(f"Create one with: agent-vm create --branch={branch}")
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

        vm_configs = []
        for config_dir in self.base_dir.iterdir():
            if config_dir.is_dir():
                config_file = config_dir / "config.json"
                if config_file.exists():
                    try:
                        with config_file.open() as f:
                            config_data = json.load(f)
                        created_at = config_data.get("created_at", "unknown")
                        vm_configs.append((config_dir.name, created_at))
                    except (json.JSONDecodeError, OSError):
                        vm_configs.append((config_dir.name, "invalid config"))

        if vm_configs:
            logger.info("Available VM configurations:")
            for name, created_at in vm_configs:
                logger.info(f"  {name} (created: {created_at})")
        else:
            logger.info("No VM configurations found")

    def destroy_vm(self, branch: Optional[str] = None) -> None:
        """Destroy VM configuration."""
        if branch is None:
            branch = self._get_current_branch()

        vm_config_dir = self.base_dir / branch

        if not vm_config_dir.exists():
            logger.error(f"No VM configuration found for branch: {branch}")
            sys.exit(1)

        logger.info(f"Destroying VM configuration for branch: {branch}")

        # Stop VM if running
        config_file = vm_config_dir / "config.json"
        if config_file.exists():
            try:
                with config_file.open() as f:
                    config_data = json.load(f)
                vm_name = config_data.get("vm_name", f"agent-dev-{branch}")

                if self._is_vm_running(vm_name):
                    logger.info("Stopping running VM before destroying configuration...")
                    self.stop_vm(branch)
                else:
                    logger.info("VM is not running")
            except (json.JSONDecodeError, subprocess.CalledProcessError):
                logger.warning("Could not check VM status before destruction")

        try:
            shutil.rmtree(vm_config_dir)
            logger.info(f"VM configuration destroyed: {vm_config_dir}")
        except OSError as e:
            logger.error(f"Failed to destroy VM configuration: {e}")
            sys.exit(1)

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
  agent-vm restart feature-x
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
    create_parser.add_argument('--config', default='vm-config.nix', help='Path to VM NixOS config')

    # Start command
    start_parser = subparsers.add_parser('start', help='Start VM for branch')
    start_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Stop command
    stop_parser = subparsers.add_parser('stop', help='Stop VM for branch')
    stop_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

    # Restart command
    restart_parser = subparsers.add_parser('restart', help='Restart VM for branch')
    restart_parser.add_argument('branch', nargs='?', help='Branch name (default: current branch)')

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
        elif args.command == 'restart':
            controller.restart_vm(args.branch)
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
