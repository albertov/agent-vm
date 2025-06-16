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
            try:
                result = subprocess.run(
                    ["git", "rev-parse", "--show-toplevel"],
                    capture_output=True,
                    text=True,
                    check=True
                )
                return result.stdout.strip()
            except subprocess.CalledProcessError:
                # If git is not available, return a default
                return "/workspace"

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
            if not branch:  # Detached HEAD state
                result = subprocess.run(
                    ["git", "rev-parse", "--short", "HEAD"],
                    capture_output=True,
                    text=True,
                    check=True
                )
                return f"detached-{result.stdout.strip()}"
            return branch
        except subprocess.CalledProcessError:
            # If git is not available, return a default
            return "default"

    def _cleanup_stale_processes(self, vm_name: str) -> None:
        """Clean up any stale QEMU processes for this VM."""
        try:
            result = subprocess.run(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True,
                text=True
            )
            if result.returncode == 0:
                pids = result.stdout.strip().split('\n')
                for pid in pids:
                    if pid:
                        try:
                            pid_int = int(pid)
                            logger.info(f"Cleaning up stale process {pid_int}")
                            os.kill(pid_int, 15)  # SIGTERM
                            time.sleep(2)
                            try:
                                os.kill(pid_int, 0)  # Check if still exists
                                os.kill(pid_int, 9)  # SIGKILL if still running
                            except ProcessLookupError:
                                pass  # Process already dead
                        except (ValueError, ProcessLookupError):
                            pass
        except subprocess.CalledProcessError:
            pass  # No stale processes found

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

        try:
            subprocess.run([
                "ssh-keygen", "-t", "ed25519", "-f", str(ssh_private_key),
                "-N", "", "-C", f"vm-key-{branch}"
            ], capture_output=True, check=True)

            ssh_private_key.chmod(0o600)
            ssh_public_key.chmod(0o644)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to generate SSH key: {e}")
            # Clean up partial creation
            shutil.rmtree(vm_config_dir, ignore_errors=True)
            sys.exit(1)

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
            # Clean up partial creation
            shutil.rmtree(vm_config_dir, ignore_errors=True)
            sys.exit(1)

        # Set up git remotes in workspace
        logger.info("Setting up git remotes...")
        try:
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

        except (subprocess.CalledProcessError, OSError) as e:
            logger.error(f"Failed to set up git remotes: {e}")
            # Clean up partial creation
            shutil.rmtree(vm_config_dir, ignore_errors=True)
            sys.exit(1)

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
            "upstream_repo": upstream_url if 'upstream_url' in locals() else "",
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

        # Clean up any stale processes first
        self._cleanup_stale_processes(vm_name)

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
              flake = builtins.getFlake "path:{workspace_dir}";
              pkgs = flake.legacyPackages.x86_64-linux;
              vm = pkgs.nixos {{
                imports = [ {config_data["config_path"]} ];
                users.users.dev.openssh.authorizedKeys.keys = [ "{ssh_public_key}" ];
                virtualisation.vmVariant.virtualisation.sharedDirectories.workspace.source = "{workspace_dir}";
                services.agent-mcp.shell = pkgs.mkMCPDevServers {{
                  name = "agent";
                  shell = flake.devShells.x86_64-linux.default;
                }};
              }};
            in vm.config.system.build.vm'''
        ]

        try:
            result = subprocess.run(vm_build_cmd, capture_output=True, text=True, check=True)
            vm_path = result.stdout.strip()

            if not vm_path:
                logger.error("Failed to build VM")
                sys.exit(1)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to build VM: {e}")
            logger.error(f"stderr: {e.stderr}")
            sys.exit(1)

        # Start VM in background
        logger.info("Starting VM...")
        vm_cmd = [f"{vm_path}/bin/run-nixos-vm"]

        env = os.environ.copy()
        env["QEMU_OPTS"] = f"-name {vm_name}"

        try:
            vm_process = subprocess.Popen(vm_cmd, env=env)
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to start VM: {e}")
            sys.exit(1)

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
                with config_file.open() as f:
                    config_data = json.load(f)
                vm_name = config_data["vm_name"]
                if self._is_vm_running(vm_name):
                    self.stop_vm(branch)
            except (json.JSONDecodeError, OSError, subprocess.CalledProcessError):
                pass  # Ignore if already stopped or config corrupted

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

            # Get process details
            try:
                pid = self._get_vm_pid(vm_name)
                if pid:
                    resources = self._get_vm_resources(pid)
                    logger.info(f"VM PID: {pid}")
                    logger.info(f"CPU: {resources.get('cpu', 'unknown')}%")
                    logger.info(f"Memory: {resources.get('memory', 'unknown')}%")
                    logger.info(f"Uptime: {resources.get('uptime', 'unknown')}")
            except Exception:
                pass

            logger.info("SSH Access: ssh -p 2222 dev@localhost")
            logger.info("MCP Proxy: http://localhost:8000 (if agent is running)")

            # Check SSH connectivity
            ssh_status = self._check_ssh_connectivity(ssh_key_path)
            if ssh_status['connected']:
                logger.info("SSH: Connected")
            else:
                logger.warning(f"SSH: Failed - {ssh_status.get('error', 'Unknown error')}")

            # Check if agent is running in VM
            agent_status = self._check_agent_service_detailed(ssh_key_path)
            if agent_status['active']:
                logger.info(f"Agent Status: Running (PID: {agent_status.get('pid', 'unknown')})")
                logger.info(f"Agent Uptime: {agent_status.get('uptime', 'unknown')}")
                logger.info(f"Agent Memory: {agent_status.get('memory', 'unknown')}")
                logger.info(f"Restart Count: {agent_status.get('restart_count', 'unknown')}")
            else:
                logger.warning("Agent Status: Not running")

            # Check MCP proxy health
            mcp_status = self._check_mcp_proxy_health(ssh_key_path, config_data.get('port', 8000))
            if mcp_status['healthy']:
                logger.info(f"MCP Proxy: Healthy (response time: {mcp_status.get('response_time', 'unknown')}ms)")
            else:
                logger.warning(f"MCP Proxy: Unhealthy - {mcp_status.get('error', 'Unknown error')}")

            # Check workspace status
            workspace_status = self._check_workspace_status(ssh_key_path, config_data.get('workspace_path', '/workspace'))
            if workspace_status['accessible']:
                logger.info(f"Workspace: Accessible ({workspace_status.get('size', 'unknown')})")
                logger.info(f"Git Status: {workspace_status.get('git_status', 'unknown')}")
            else:
                logger.warning(f"Workspace: Not accessible - {workspace_status.get('error', 'Unknown error')}")

        else:
            logger.info("VM Status: Stopped")

    def _get_vm_pid(self, vm_name: str) -> Optional[int]:
        """Get VM process PID."""
        try:
            result = subprocess.run(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True,
                text=True,
                check=True
            )
            return int(result.stdout.strip().split('\n')[0])
        except (subprocess.CalledProcessError, ValueError):
            return None

    def _get_vm_resources(self, pid: int) -> Dict[str, str]:
        """Get VM resource usage."""
        try:
            result = subprocess.run(
                ["ps", "-p", str(pid), "-o", "pid,ppid,%cpu,%mem,etime,comm"],
                capture_output=True,
                text=True,
                check=True
            )
            lines = result.stdout.strip().split('\n')
            if len(lines) > 1:
                parts = lines[1].split()
                if len(parts) >= 6:
                    return {
                        'cpu': parts[2],
                        'memory': parts[3],
                        'uptime': parts[4]
                    }
        except subprocess.CalledProcessError:
            pass
        return {}

    def _check_ssh_connectivity(self, ssh_key_path: Path) -> Dict[str, any]:
        """Check SSH connectivity to VM."""
        try:
            ssh_cmd = [
                "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", "2222", "dev@localhost", "echo 'SSH OK'"
            ]
            result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
            if result.returncode == 0:
                return {'connected': True}
            else:
                return {'connected': False, 'error': result.stderr.decode()}
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            return {'connected': False, 'error': str(e)}

    def _check_agent_service_detailed(self, ssh_key_path: Path) -> Dict[str, any]:
        """Check detailed agent service status."""
        try:
            ssh_cmd = [
                "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", "2222", "dev@localhost",
                "systemctl show agent-mcp --property=ActiveState,SubState,MainPID,NRestarts,MemoryCurrent"
            ]
            result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
            if result.returncode == 0:
                properties = {}
                for line in result.stdout.decode().strip().split('\n'):
                    if '=' in line:
                        key, value = line.split('=', 1)
                        properties[key] = value

                active = properties.get('ActiveState') == 'active'
                memory_bytes = int(properties.get('MemoryCurrent', '0'))
                memory_mb = f"{memory_bytes // (1024 * 1024)} MB" if memory_bytes > 0 else "0 MB"

                return {
                    'active': active,
                    'substate': properties.get('SubState', 'unknown'),
                    'pid': properties.get('MainPID', 'unknown'),
                    'restart_count': properties.get('NRestarts', 'unknown'),
                    'memory': memory_mb
                }
            else:
                return {'active': False, 'error': result.stderr.decode()}
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            return {'active': False, 'error': str(e)}

    def _check_mcp_proxy_health(self, ssh_key_path: Path, port: int) -> Dict[str, any]:
        """Check MCP proxy health."""
        try:
            start_time = time.time()
            ssh_cmd = [
                "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", "2222", "dev@localhost",
                f"curl -f http://localhost:{port}/health"
            ]
            result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
            response_time = int((time.time() - start_time) * 1000)  # Convert to ms

            if result.returncode == 0:
                return {'healthy': True, 'response_time': response_time}
            else:
                return {'healthy': False, 'error': result.stderr.decode(), 'response_time': response_time}
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            return {'healthy': False, 'error': str(e)}

    def _check_workspace_status(self, ssh_key_path: Path, workspace_path: str) -> Dict[str, any]:
        """Check workspace accessibility and git status."""
        try:
            ssh_cmd = [
                "ssh", "-o", "ConnectTimeout=5", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", "2222", "dev@localhost",
                f"cd {workspace_path} && pwd && du -sh . && git status --porcelain | wc -l"
            ]
            result = subprocess.run(ssh_cmd, capture_output=True, timeout=10)
            if result.returncode == 0:
                lines = result.stdout.decode().strip().split('\n')
                if len(lines) >= 3:
                    size = lines[1]
                    git_changes = int(lines[2]) if lines[2].isdigit() else 0
                    git_status = "clean" if git_changes == 0 else f"{git_changes} modified files"

                    return {
                        'accessible': True,
                        'size': size,
                        'git_status': git_status
                    }
            return {'accessible': False, 'error': result.stderr.decode()}
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            return {'accessible': False, 'error': str(e)}


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
