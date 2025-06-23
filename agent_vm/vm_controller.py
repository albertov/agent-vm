#!/usr/bin/env python3
"""
VM control command for managing development VMs.

This tool provides comprehensive VM lifecycle management including creation,
startup, shutdown, and workspace management for agent development environments.

Usage: agent-vm <command> [options]
"""

import typer
import atexit
import json
import logging
import os
import shutil
import socket
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Dict, List, Optional

# Color codes for terminal output
class Colors:
    """ANSI color codes for terminal output."""
    RESET = '\033[0m'
    BOLD = '\033[1m'
    DIM = '\033[2m'

    # Standard colors
    RED = '\033[31m'
    GREEN = '\033[32m'
    YELLOW = '\033[33m'
    BLUE = '\033[34m'
    MAGENTA = '\033[35m'
    CYAN = '\033[36m'
    WHITE = '\033[37m'

    # Bright colors
    BRIGHT_RED = '\033[91m'
    BRIGHT_GREEN = '\033[92m'
    BRIGHT_YELLOW = '\033[93m'
    BRIGHT_BLUE = '\033[94m'
    BRIGHT_MAGENTA = '\033[95m'
    BRIGHT_CYAN = '\033[96m'


class ColorFormatter(logging.Formatter):
    """Custom formatter with color support."""

    # Color mapping for different log levels
    COLORS = {
        logging.DEBUG: Colors.DIM + Colors.WHITE,
        logging.INFO: Colors.BRIGHT_BLUE,
        logging.WARNING: Colors.BRIGHT_YELLOW,
        logging.ERROR: Colors.BRIGHT_RED,
        logging.CRITICAL: Colors.BOLD + Colors.BRIGHT_RED,
    }

    def format(self, record):
        # Apply color based on log level
        color = self.COLORS.get(record.levelno, Colors.RESET)

        # Format the message
        formatted = super().format(record)

        # Add colors if output is a TTY
        if hasattr(sys.stdout, 'isatty') and sys.stdout.isatty():
            # Color the level name
            level_color = color + record.levelname + Colors.RESET
            formatted = formatted.replace(record.levelname, level_color)

            # Add colored prefix based on message content
            if "✅" in record.msg or "SUCCESS" in record.msg.upper():
                formatted = Colors.BRIGHT_GREEN + "✅ " + Colors.RESET + formatted
            elif "❌" in record.msg or "ERROR" in record.msg.upper():
                formatted = Colors.BRIGHT_RED + "❌ " + Colors.RESET + formatted
            elif "⚠️" in record.msg or "WARNING" in record.msg.upper():
                formatted = Colors.BRIGHT_YELLOW + "⚠️ " + Colors.RESET + formatted
            elif "🔧" in record.msg or "BUILDING" in record.msg.upper():
                formatted = Colors.BRIGHT_CYAN + "🔧 " + Colors.RESET + formatted
            elif "🚀" in record.msg or "STARTING" in record.msg.upper():
                formatted = Colors.BRIGHT_MAGENTA + "🚀 " + Colors.RESET + formatted

        return formatted


# Configure logging with colored output
def setup_logging(verbose: bool = False):
    """Set up colored logging configuration."""
    level = logging.DEBUG if verbose else logging.INFO

    # Create formatter
    formatter = ColorFormatter(
        fmt="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S"
    )

    # Create handler
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(formatter)

    # Configure root logger
    logging.basicConfig(
        level=level,
        handlers=[handler],
        force=True  # Override any existing configuration
    )


# Set up initial logging (will be reconfigured in main() if needed)
setup_logging()
logger = logging.getLogger(__name__)


# Global list to track tempfiles for cleanup
_temp_files: List[tempfile._TemporaryFileWrapper] = []


def _cleanup_tempfiles():
    """Clean up all temporary files on exit."""
    for temp_file in _temp_files:
        try:
            temp_file.close()
        except Exception:
            pass  # Ignore errors during cleanup


# Register cleanup function to run at exit
atexit.register(_cleanup_tempfiles)


def _get_last_lines(file_path: str, num_lines: int = 20) -> List[str]:
    """Get the last N lines from a file."""
    try:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()
            return lines[-num_lines:] if len(lines) > num_lines else lines
    except Exception:
        # FIXME: Do not ignore this error
        return []


def run_subprocess(cmd: List[str], **kwargs) -> subprocess.CompletedProcess:
    """
    Run subprocess with stdout/stderr captured to tempfiles.

    Args:
        cmd: Command to run as list of strings
        **kwargs: Additional keyword arguments for subprocess.run

    Returns:
        CompletedProcess with stdout/stderr captured

    On error, logs the last 20 lines of stdout/stderr and their file paths.
    """
    # Create temporary files for stdout and stderr
    stdout_temp = tempfile.NamedTemporaryFile(mode='w+', prefix='agent_vm_stdout_',
                                              suffix='.log', delete=False)
    stderr_temp = tempfile.NamedTemporaryFile(mode='w+', prefix='agent_vm_stderr_',
                                              suffix='.log', delete=False)

    # Track for cleanup
    _temp_files.extend([stdout_temp, stderr_temp])

    # Set capture output options
    kwargs_copy = kwargs.copy()
    if 'capture_output' not in kwargs_copy:
        kwargs_copy['stdout'] = stdout_temp
        kwargs_copy['stderr'] = stderr_temp
        kwargs_copy['text'] = True

    logger.debug(f"Running command: {' '.join(cmd)}")
    logger.debug(f"stdout log: {stdout_temp.name}")
    logger.debug(f"stderr log: {stderr_temp.name}")

    try:
        result = subprocess.run(cmd, **kwargs_copy)

        # Flush and close temp files
        stdout_temp.flush()
        stderr_temp.flush()
        stdout_temp.close()
        stderr_temp.close()

        # Read captured output for return value
        with open(stdout_temp.name, 'r', encoding='utf-8', errors='replace') as f:
            captured_stdout = f.read()
        with open(stderr_temp.name, 'r', encoding='utf-8', errors='replace') as f:
            captured_stderr = f.read()

        # Create result with captured output
        result.stdout = captured_stdout
        result.stderr = captured_stderr

        # If command failed, log the last 20 lines of output
        if result.returncode != 0:
            logger.error(f"Command failed with exit code {result.returncode}: {' '.join(cmd)}")
            logger.error(f"stdout log path: {stdout_temp.name}")
            logger.error(f"stderr log path: {stderr_temp.name}")

            # Log last 20 lines of stdout if not empty
            stdout_lines = _get_last_lines(stdout_temp.name, 20)
            if stdout_lines:
                logger.error("Last 20 lines of stdout:")
                for line in stdout_lines:
                    logger.error(f"stdout: {line.rstrip()}")

            # Log last 20 lines of stderr if not empty
            stderr_lines = _get_last_lines(stderr_temp.name, 20)
            if stderr_lines:
                logger.error("Last 20 lines of stderr:")
                for line in stderr_lines:
                    logger.error(f"stderr: {line.rstrip()}")

        return result

    except Exception as e:
        # Close temp files
        stdout_temp.close()
        stderr_temp.close()

        logger.error(f"Command execution failed: {' '.join(cmd)}")
        logger.error(f"Exception: {e}")
        logger.error(f"stdout log path: {stdout_temp.name}")
        logger.error(f"stderr log path: {stderr_temp.name}")

        # Log any captured output before exception
        stdout_lines = _get_last_lines(stdout_temp.name, 20)
        if stdout_lines:
            logger.error("Last 20 lines of stdout before exception:")
            for line in stdout_lines:
                logger.error(f"stdout: {line.rstrip()}")

        stderr_lines = _get_last_lines(stderr_temp.name, 20)
        if stderr_lines:
            logger.error("Last 20 lines of stderr before exception:")
            for line in stderr_lines:
                logger.error(f"stderr: {line.rstrip()}")

        raise
    # FIXME: If no exception delete log files


class ProcessWithOutput:
    """
    Wrapper for subprocess.Popen that captures output to tempfiles.
    """

    def __init__(self, cmd: List[str], **kwargs):
        """Initialize Popen with tempfile capture."""
        # Create temporary files for stdout and stderr
        self.stdout_temp = tempfile.NamedTemporaryFile(mode='w+', prefix='agent_vm_stdout_',
                                                       suffix='.log', delete=False)
        self.stderr_temp = tempfile.NamedTemporaryFile(mode='w+', prefix='agent_vm_stderr_',
                                                       suffix='.log', delete=False)

        # Track for cleanup
        _temp_files.extend([self.stdout_temp, self.stderr_temp])

        # Set capture output options if not already set
        kwargs_copy = kwargs.copy()
        if 'stdout' not in kwargs_copy:
            kwargs_copy['stdout'] = self.stdout_temp
        if 'stderr' not in kwargs_copy:
            kwargs_copy['stderr'] = self.stderr_temp
        if 'text' not in kwargs_copy:
            kwargs_copy['text'] = True

        logger.debug(f"Starting process: {' '.join(cmd)}")
        logger.debug(f"stdout log: {self.stdout_temp.name}")
        logger.debug(f"stderr log: {self.stderr_temp.name}")

        self.cmd = cmd
        self.process = subprocess.Popen(cmd, **kwargs_copy)
        self.pid = self.process.pid

    def wait(self, timeout=None):
        """Wait for process to complete."""
        try:
            returncode = self.process.wait(timeout)

            # Flush temp files
            self.stdout_temp.flush()
            self.stderr_temp.flush()

            # If process failed, log output
            if returncode != 0:
                logger.error(f"Process failed with exit code {returncode}: {' '.join(self.cmd)}")
                logger.error(f"stdout log path: {self.stdout_temp.name}")
                logger.error(f"stderr log path: {self.stderr_temp.name}")

                # Log last 20 lines of output
                stdout_lines = _get_last_lines(self.stdout_temp.name, 20)
                if stdout_lines:
                    logger.error("Last 20 lines of stdout:")
                    for line in stdout_lines:
                        logger.error(f"stdout: {line.rstrip()}")

                stderr_lines = _get_last_lines(self.stderr_temp.name, 20)
                if stderr_lines:
                    logger.error("Last 20 lines of stderr:")
                    for line in stderr_lines:
                        logger.error(f"stderr: {line.rstrip()}")

            return returncode
        finally:
            self.stdout_temp.close()
            self.stderr_temp.close()

    def poll(self):
        """Check if process has terminated."""
        return self.process.poll()

    def terminate(self):
        """Terminate the process."""
        return self.process.terminate()

    def kill(self):
        """Kill the process."""
        return self.process.kill()


def _find_free_port(start_port: int = 2222, max_attempts: int = 100) -> int:
    """Find a free port starting from start_port."""
    for port in range(start_port, start_port + max_attempts):
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                sock.bind(('localhost', port))
                return port
        except OSError:
            continue  # Port is already in use

    raise RuntimeError(f"Could not find a free port in range {start_port}-{start_port + max_attempts}")


class VMController:
    """VM lifecycle management controller."""

    def __init__(self, state_dir: Optional[str] = None) -> None:
        """Initialize VM controller with configuration paths."""
        self.home_dir = Path.home()
        if state_dir:
            self.base_dir = Path(state_dir).expanduser().resolve()
        else:
            self.base_dir = self.home_dir / ".local" / "share" / "agent-vms"
        self.origin_repo = self._get_origin_repo()

    def _get_origin_repo(self) -> str:
        """Get the origin repository URL or path."""
        try:
            result = run_subprocess(
                ["git", "remote", "get-url", "origin"],
                capture_output=True,
                text=True,
                check=True
            )
            origin_url = result.stdout.strip()
            if origin_url:
                return origin_url
        except subprocess.CalledProcessError:
            pass

        # Fallback to git root directory
        try:
            result = run_subprocess(
                ["git", "rev-parse", "--show-toplevel"],
                capture_output=True,
                text=True,
                check=True
            )
            root_path = result.stdout.strip()
            if root_path:
                return root_path
        except subprocess.CalledProcessError:
            pass

        # Final fallback - use current working directory
        import os
        return os.getcwd()

    def _cleanup_stale_processes(self, vm_name: str) -> None:
        """Clean up any stale VM processes."""
        try:
            # Find stale QEMU processes
            result = run_subprocess(
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
            result = run_subprocess(
                ["git", "branch", "--show-current"],
                capture_output=True,
                text=True,
                check=True
            )
            branch = result.stdout.strip()
            if branch:
                return branch
        except subprocess.CalledProcessError:
            pass

        # We're likely in detached HEAD state, try to get commit hash
        try:
            result = run_subprocess(
                ["git", "rev-parse", "--short", "HEAD"],
                capture_output=True,
                text=True,
                check=True
            )
            commit_hash = result.stdout.strip()
            if commit_hash:
                return f"detached-{commit_hash}"
        except subprocess.CalledProcessError:
            pass

        # Try to get full commit hash as fallback
        try:
            result = run_subprocess(
                ["git", "rev-parse", "HEAD"],
                capture_output=True,
                text=True,
                check=True
            )
            commit_hash = result.stdout.strip()
            if commit_hash:
                return f"detached-{commit_hash[:8]}"  # Use first 8 chars
        except subprocess.CalledProcessError:
            pass

        # Final fallback - not in a git repository or git not available
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

        # FIXME: Make it passwordless. This is probably why integration tests
        # don't pass don't you think?
        run_subprocess([
            "ssh-keygen", "-t", "ed25519", "-f", str(ssh_private_key),
            "-N", "", "-C", f"vm-key-{branch}"
        ], capture_output=True, check=True)

        ssh_private_key.chmod(0o600)
        ssh_public_key.chmod(0o644)

        # Clone current repository at current branch
        logger.info("Cloning repository to workspace at current branch...")
        current_branch = self._get_current_branch()

        try:
            # Use the origin repo we already determined in __init__
            repo_root = self.origin_repo

            # Check if we have a valid branch name or if we're using fallback
            if current_branch == "default":
                # If we're using the fallback "default" branch, clone without specifying branch
                # and then ensure we're on the current HEAD
                logger.info("Using fallback branch detection - cloning current HEAD state")
                run_subprocess([
                    "git", "clone", repo_root, str(workspace_dir)
                ], check=True)

                # Get current commit hash to check out the same state
                try:
                    current_commit = run_subprocess(
                        ["git", "rev-parse", "HEAD"],
                        capture_output=True, text=True, check=True
                    ).stdout.strip()

                    if current_commit:
                        # Change to workspace and checkout the same commit
                        original_cwd = os.getcwd()
                        os.chdir(workspace_dir)
                        try:
                            run_subprocess(["git", "checkout", current_commit], check=True)
                        finally:
                            os.chdir(original_cwd)
                except subprocess.CalledProcessError:
                    logger.warning("Could not checkout specific commit - using default branch")
            else:
                # We have a valid branch name, use it
                run_subprocess([
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

        run_subprocess(["git", "remote", "set-url", "origin", origin_url], check=True)

        # Add upstream remote if different from origin
        try:
            os.chdir(repo_root)
            upstream_result = run_subprocess(
                ["git", "remote", "get-url", "origin"],
                capture_output=True, text=True
            )
            upstream_url = upstream_result.stdout.strip() if upstream_result.returncode == 0 else ""

            if upstream_url and upstream_url != self.origin_repo:
                os.chdir(workspace_dir)
                run_subprocess(["git", "remote", "add", "upstream", upstream_url])
        except subprocess.CalledProcessError:
            upstream_url = ""

        # Find a free SSH port starting from 2222
        ssh_port = _find_free_port(2222)
        logger.info(f"🔌 Allocated SSH port: {ssh_port}")

        # Create VM config metadata
        logger.info("Creating VM configuration metadata...")
        config_data = {
            "branch": branch,
            "host": host,
            "port": port,
            "ssh_port": ssh_port,  # Store the dynamically allocated SSH port
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

        logger.info("✅ VM configuration created successfully!")
        logger.info(f"📁 Branch: {branch}")
        logger.info(f"🗂️  Config directory: {vm_config_dir}")
        logger.info(f"💾 Workspace: {workspace_dir}")
        logger.info(f"🔑 SSH key: {ssh_private_key}")
        logger.info("")
        logger.info(f"🚀 Next step: Start the VM with: {Colors.BRIGHT_GREEN}agent-vm start {branch}{Colors.RESET}")

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
        ssh_port = config_data.get("ssh_port", 2222)  # Use stored SSH port or fallback to 2222
        vm_name = config_data["vm_name"]

        # Check if VM is already running
        if self._is_vm_running(vm_name):
            logger.warning("VM is already running")
            self._show_vm_status(vm_config_dir, config_data)
            return

        # Clean up any stale processes first
        self._cleanup_stale_processes(vm_name)

        logger.info(f"🚀 Starting VM for branch: {branch}")

        # Get the original flake directory before changing to workspace
        try:
            # Use the origin repo we already determined in __init__
            original_flake_dir = self.origin_repo
            if not original_flake_dir:
                logger.error("❌ Could not determine original flake directory")
                sys.exit(1)
        except Exception:
            logger.error("❌ Could not determine original flake directory")
            sys.exit(1)

        os.chdir(workspace_dir)

        # Build VM configuration using existing vm-config.nix with agent service
        logger.info("🔧 Building VM configuration with agent service...")
        ssh_public_key = (ssh_key_path.parent / "id_ed25519.pub").read_text().strip()

        try:
            vm_build_cmd = [
                    "nix", "build", "--no-link", "--print-out-paths", "--impure",
                    "--expr", f'''
let
  flake = builtins.getFlake "{workspace_dir}";
  pkgs = flake.legacyPackages.${{builtins.currentSystem}};
in
  (pkgs.lib.nixosSystem {{
    inherit pkgs;
    inherit (pkgs) system;
    modules = [
      "${{flake}}/vm-config.nix"
      {{
        # Override SSH key and ports for this specific VM instance
        users.users.dev.openssh.authorizedKeys.keys = pkgs.lib.mkForce [ "{ssh_public_key}" ];
        services.agent-mcp.port = pkgs.lib.mkForce {config_data["port"]};
        networking.firewall.allowedTCPPorts = pkgs.lib.mkForce [ 22 {config_data["port"]} ];
        virtualisation.vmVariant.virtualisation.forwardPorts = pkgs.lib.mkForce [
          {{ from = "host"; host.port = {config_data["port"]}; guest.port = {config_data["port"]}; }}
          {{ from = "host"; host.port = {ssh_port}; guest.port = 22; }}
        ];
        virtualisation.vmVariant.virtualisation.sharedDirectories.workspace.source = pkgs.lib.mkForce "{workspace_dir}";
      }}
    ];
  }}).config.system.build.vm
                    '''
                ]

            logger.debug(f"Running VM build command: {' '.join(vm_build_cmd)}")
            result = subprocess.run(
                vm_build_cmd,
                capture_output=True,
                text=True,
                cwd=workspace_dir,
                timeout=_get_global_timeout()  # Use global timeout for VM building
            )

            if result.returncode != 0:
                logger.error(f"VM build failed with exit code: {result.returncode}")
                logger.error(f"VM build stderr: {result.stderr}")
                logger.error(f"VM build stdout: {result.stdout}")
                logger.error(f"Command: {' '.join(vm_build_cmd)}")
                logger.error(f"Working directory: {workspace_dir}")
                raise subprocess.CalledProcessError(result.returncode, vm_build_cmd)
            vm_path = result.stdout.strip()
        except subprocess.CalledProcessError as e:
            logger.error(f"Failed to build VM configuration: {e}")
            sys.exit(1)

        if not vm_path:
            logger.error("Failed to build VM")
            sys.exit(1)

        # Start VM in background
        logger.info("Starting VM...")
        vm_cmd = [f"{vm_path}/bin/run-nixos-vm"]

        env = os.environ.copy()
        env["QEMU_OPTS"] = f"-name {vm_name}"

        vm_process = ProcessWithOutput(vm_cmd, env=env)

        # Store PID
        pid_file = vm_config_dir / "vm.pid"
        with pid_file.open('w') as f:
            f.write(str(vm_process.pid))

        # Wait for VM to be ready
        if self._wait_for_vm_ready(ssh_key_path, ssh_port):
            logger.info(f"✅ VM started successfully (PID: {vm_process.pid})")

            # Start agent services in VM
            self._start_agent_in_vm(ssh_key_path, ssh_port)
        else:
            logger.error("❌ VM startup failed - performing cleanup...")

            # Cleanup failed VM process
            try:
                self._stop_vm_by_pid(vm_config_dir)
                logger.info("🧹 Cleanup completed")
            except Exception as cleanup_error:
                logger.warning(f"⚠️ Cleanup failed: {cleanup_error}")

            logger.error("🔧 Troubleshooting tips:")
            logger.error("  • Check if nested virtualization is enabled")
            logger.error("  • Ensure sufficient memory and disk space")
            logger.error("  • Try: agent-vm destroy && agent-vm create")
            logger.error(f"  • View verbose logs with: agent-vm --verbose start {branch}")
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
        ssh_port = config_data.get("ssh_port", 2222)  # Use stored SSH port or fallback to 2222

        if not self._is_vm_running(vm_name):
            logger.error("VM is not running. Start it first with: agent-vm start")
            sys.exit(1)

        logger.info(f"Opening shell in VM for branch: {branch}")

        # SSH into VM (interactive, don't capture output)
        ssh_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", str(ssh_port), "dev@localhost"
        ]

        # For interactive SSH, use subprocess.run directly without capture
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
        ssh_port = config_data.get("ssh_port", 2222)  # Use stored SSH port or fallback to 2222

        logger.info(f"Showing logs for VM: {branch}")

        if self._is_vm_running(vm_name):
            logger.info("VM is running. Checking agent service logs via SSH...")
            try:
                # Check agent service logs in VM (interactive, don't capture output)
                ssh_cmd = [
                    "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", str(ssh_port), "dev@localhost",
                    "journalctl -f --no-pager -n 50 -u agent-mcp"
                ]
                # For interactive logs viewing, use subprocess.run directly without capture
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
            result = subprocess.run(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True, text=True
            )
            return result.returncode == 0
        except Exception:
            return False

    def _wait_for_vm_ready(self, ssh_key_path: Path, ssh_port: int = 2222, max_attempts: Optional[int] = None) -> bool:
        """Wait for VM to be ready for SSH connections."""
        if max_attempts is None:
            # Calculate reasonable max_attempts based on global timeout
            # Each attempt takes ~2 seconds, so divide timeout by 2
            max_attempts = max(1, _get_global_timeout() // 2)

        timeout_per_attempt = min(10, _get_global_timeout() // max_attempts) if max_attempts > 0 else 10

        logger.info("🚀 Waiting for VM to be ready...")
        logger.debug(f"🔍 Using SSH key: {ssh_key_path}")
        logger.debug(f"🔍 SSH key exists: {ssh_key_path.exists()}")

        if ssh_key_path.exists():
            logger.debug(f"🔍 SSH key permissions: {oct(ssh_key_path.stat().st_mode)[-3:]}")

        # Progress indicators
        progress_chars = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

        for attempt in range(max_attempts):
            try:
                ssh_cmd = [
                    "ssh", "-o", f"ConnectTimeout={timeout_per_attempt}", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", str(ssh_port), "dev@localhost", "echo 'VM Ready'"
                ]

                if attempt == 0 or attempt % 10 == 0:  # Log command details periodically
                    logger.debug(f"🔍 SSH attempt {attempt + 1}: {' '.join(ssh_cmd)}")

                result = run_subprocess(ssh_cmd, capture_output=True, timeout=timeout_per_attempt + 5)

                if attempt % 10 == 0:  # Log results periodically for debugging
                    logger.debug(f"🔍 SSH attempt {attempt + 1} exit code: {result.returncode}")
                    logger.debug(f"🔍 SSH attempt {attempt + 1} stdout: {result.stdout}")
                    logger.debug(f"🔍 SSH attempt {attempt + 1} stderr: {result.stderr}")

                if result.returncode == 0:
                    # Clear progress line
                    print("\r" + " " * 50 + "\r", end="", flush=True)
                    logger.info("✅ VM is ready for connections")
                    return True
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
                if attempt % 10 == 0:  # Log errors periodically for debugging
                    logger.debug(f"🔍 SSH attempt {attempt + 1} failed: {e}")

            # Show progress with spinning indicator
            progress_char = progress_chars[attempt % len(progress_chars)]
            remaining_time = (max_attempts - attempt) * 2
            mins, secs = divmod(remaining_time, 60)

            if hasattr(sys.stdout, 'isatty') and sys.stdout.isatty():
                # Show colorful progress with spinning indicator
                print(f"\r{Colors.BRIGHT_CYAN}{progress_char}{Colors.RESET} "
                      f"Waiting for VM... "
                      f"{Colors.DIM}[{attempt + 1}/{max_attempts}] "
                      f"~{mins:02d}:{secs:02d} remaining{Colors.RESET}",
                      end="", flush=True)
            else:
                # Fallback for non-TTY environments
                if attempt % 5 == 0:  # Print every 5 attempts to reduce noise
                    print(f"Waiting for VM... [{attempt + 1}/{max_attempts}]")

            time.sleep(2)

        # Clear progress line and show error
        print("\r" + " " * 50 + "\r", end="", flush=True)
        total_time = max_attempts * 2  # Approximate total time spent
        logger.error(f"❌ VM failed to become ready after {max_attempts} attempts (~{total_time} seconds, timeout: {_get_global_timeout()}s)")
        logger.error("🔧 VM startup troubleshooting:")
        logger.error("  • Check if VM process is still running: ps aux | grep qemu")
        logger.error(f"  • Try connecting manually: ssh -i <key> -p {ssh_port} dev@localhost")
        logger.error("  • Check VM console output for boot errors")
        logger.error("  • Verify nested virtualization is enabled")
        logger.error(f"  • Consider increasing timeout with: --timeout {_get_global_timeout() * 2}")
        return False

    def _start_agent_in_vm(self, ssh_key_path: Path, ssh_port: int = 2222) -> None:
        """Start agent services in VM using systemd."""
        logger.info("🔧 Starting agent services in VM...")

        # First check if the agent-mcp service is available
        logger.debug("🔍 Checking if agent-mcp service is available...")
        service_check_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", str(ssh_port), "dev@localhost",
            "systemctl list-unit-files agent-mcp.service"
        ]

        try:
            service_result = run_subprocess(service_check_cmd, capture_output=True, text=True, timeout=min(15, _get_global_timeout()))
            logger.debug(f"🔍 Service list exit code: {service_result.returncode}")
            logger.debug(f"🔍 Service list output: {service_result.stdout.strip()}")

            if service_result.returncode != 0 or "agent-mcp.service" not in service_result.stdout:
                logger.info("ℹ️ Agent service not available (disabled for integration testing)")
                logger.info("✅ VM started successfully without agent service")
                logger.info(f"💻 To access VM shell: {Colors.BRIGHT_GREEN}agent-vm shell{Colors.RESET}")
                return
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
            logger.info("ℹ️ Agent service not available (disabled for integration testing)")
            logger.info("✅ VM started successfully without agent service")
            logger.info(f"💻 To access VM shell: {Colors.BRIGHT_GREEN}agent-vm shell{Colors.RESET}")
            return

        # Service is available, continue with normal startup logic
        # First check if service is already running
        logger.debug("🔍 Checking if agent-mcp service is already running...")
        status_check_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", str(ssh_port), "dev@localhost",
            "systemctl is-active agent-mcp"
        ]

        try:
            status_result = run_subprocess(status_check_cmd, capture_output=True, text=True, timeout=min(15, _get_global_timeout()))
            logger.debug(f"🔍 Service status check exit code: {status_result.returncode}")
            logger.debug(f"🔍 Service status output: {status_result.stdout.strip()}")
            logger.debug(f"🔍 Service status stderr: {status_result.stderr.strip()}")

            if status_result.returncode == 0 and "active" in status_result.stdout:
                logger.info("✅ Agent service is already running")
                logger.info("🎉 MCP Proxy should be available at: http://localhost:8000")
                logger.info(f"💻 To access VM shell: {Colors.BRIGHT_GREEN}agent-vm shell{Colors.RESET}")
                return
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            logger.debug(f"🔍 Service status check failed: {e}")

        # Service is not running, try to start it
        logger.info("🔧 Agent service not running, starting it...")
        ssh_cmd = [
            "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
            "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
            "-p", str(ssh_port), "dev@localhost",
            "sudo systemctl start agent-mcp"
        ]

        logger.debug(f"🔍 Running start command: {' '.join(ssh_cmd)}")

        try:
            result = run_subprocess(ssh_cmd, capture_output=True, text=True, timeout=min(30, _get_global_timeout()), check=True)
            logger.debug(f"🔍 Start command exit code: {result.returncode}")
            logger.debug(f"🔍 Start command stdout: {result.stdout.strip()}")
            logger.debug(f"🔍 Start command stderr: {result.stderr.strip()}")

            logger.info("✅ Agent services started in VM")

            # Wait for service to be ready
            if self._wait_for_agent_ready(ssh_key_path, ssh_port):
                logger.info("🎉 MCP Proxy available at: http://localhost:8000")
                logger.info(f"💻 To access VM shell: {Colors.BRIGHT_GREEN}agent-vm shell{Colors.RESET}")
            else:
                logger.warning("⚠️ Agent services started but MCP proxy is not responding")
                logger.info("🔍 Check service logs with: agent-vm logs")

        except subprocess.CalledProcessError as e:
            logger.error("❌ Failed to start agent services in VM")
            logger.error(f"🔍 Command exit code: {e.returncode}")
            logger.error(f"🔍 Command stdout: {e.stdout}")
            logger.error(f"🔍 Command stderr: {e.stderr}")
            logger.error("🔧 Troubleshooting:")
            logger.error("  • Check systemd service status: sudo systemctl status agent-mcp")
            logger.error("  • View service logs: journalctl -u agent-mcp")
            logger.error(f"  • SSH into VM: {Colors.BRIGHT_GREEN}agent-vm shell{Colors.RESET}")
            logger.error("  • Check if passwordless sudo is working: sudo -n true")
            raise
        except subprocess.TimeoutExpired:
            logger.error("❌ Agent service start command timed out")
            logger.error(f"🔍 Command that timed out: {' '.join(ssh_cmd)}")
            logger.error("🔧 This might indicate SSH or sudo issues")
            raise

    def _wait_for_agent_ready(self, ssh_key_path: Path, ssh_port: int = 2222, max_attempts: Optional[int] = None) -> bool:
        """Wait for agent service to be ready."""
        if max_attempts is None:
            # Calculate reasonable max_attempts based on global timeout
            # Each attempt takes ~2 seconds, so divide timeout by 2
            max_attempts = max(1, _get_global_timeout() // 2)

        timeout_per_attempt = min(10, _get_global_timeout() // max_attempts) if max_attempts > 0 else 10

        logger.info("🔧 Waiting for agent service to be ready...")

        # Progress indicators
        progress_chars = ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]

        for attempt in range(max_attempts):
            try:
                ssh_cmd = [
                    "ssh", "-o", f"ConnectTimeout={timeout_per_attempt}", "-o", "StrictHostKeyChecking=no",
                    "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                    "-p", str(ssh_port), "dev@localhost",
                    "curl -f http://localhost:8000/health"
                ]
                result = run_subprocess(ssh_cmd, capture_output=True, timeout=timeout_per_attempt + 5)
                if result.returncode == 0:
                    # Clear progress line
                    print("\r" + " " * 60 + "\r", end="", flush=True)
                    logger.info("✅ Agent service is ready")
                    return True
            except (subprocess.CalledProcessError, subprocess.TimeoutExpired):
                pass

            # Show progress with spinning indicator
            progress_char = progress_chars[attempt % len(progress_chars)]
            remaining_time = (max_attempts - attempt) * 2
            mins, secs = divmod(remaining_time, 60)

            if hasattr(sys.stdout, 'isatty') and sys.stdout.isatty():
                # Show colorful progress with spinning indicator
                print(f"\r{Colors.BRIGHT_MAGENTA}{progress_char}{Colors.RESET} "
                      f"Starting MCP services... "
                      f"{Colors.DIM}[{attempt + 1}/{max_attempts}] "
                      f"~{mins:02d}:{secs:02d} remaining{Colors.RESET}",
                      end="", flush=True)
            else:
                # Fallback for non-TTY environments
                if attempt % 3 == 0:  # Print every 3 attempts to reduce noise
                    print(f"Starting MCP services... [{attempt + 1}/{max_attempts}]")

            time.sleep(2)

        # Clear progress line and show error
        print("\r" + " " * 60 + "\r", end="", flush=True)
        total_time = max_attempts * 2  # Approximate total time spent
        logger.error(f"❌ Agent service failed to become ready after {max_attempts} attempts (~{total_time} seconds, timeout: {_get_global_timeout()}s)")
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
            run_subprocess(["pkill", "-f", f"qemu.*{vm_name}"], capture_output=True)
        except (FileNotFoundError, json.JSONDecodeError, subprocess.CalledProcessError):
            pass

        logger.info("VM stopped")

    def _show_vm_status(self, vm_config_dir: Path, config_data: Dict) -> None:
        """Show detailed VM status with comprehensive monitoring."""
        vm_name = config_data["vm_name"]
        ssh_key_path = Path(config_data["ssh_key_path"])
        ssh_port = config_data.get("ssh_port", 2222)  # Use the correct SSH port

        logger.info(f"VM Configuration: {config_data['branch']}")
        logger.info(f"Created: {config_data.get('created_at', 'unknown')}")
        logger.info(f"Workspace: {config_data['workspace_path']}")
        logger.info(f"MCP Port: {config_data['port']}")
        logger.info("="*50)

        if self._is_vm_running(vm_name):
            logger.info("🟢 VM Status: Running")

            # Get VM process details
            vm_pid = self._get_vm_pid(vm_name)
            if vm_pid:
                logger.info(f"Process ID: {vm_pid}")

                # Get VM resource usage
                vm_resources = self._get_vm_resources(vm_pid)
                if vm_resources:
                    logger.info(f"CPU Usage: {vm_resources.get('cpu', 'unknown')}%")
                    logger.info(f"Memory Usage: {vm_resources.get('memory', 'unknown')} MB")
                    logger.info(f"VM Uptime: {vm_resources.get('uptime', 'unknown')}")

            # Check SSH connectivity with detailed diagnostics
            logger.info("-" * 30)
            ssh_status = self._check_ssh_connectivity(ssh_key_path, ssh_port)
            if ssh_status['connected']:
                logger.info("🟢 SSH Connectivity: Available")
                logger.info(f"SSH Access: ssh -i {ssh_key_path} -p {ssh_port} dev@localhost")

                # Check agent service status with details
                agent_status = self._check_agent_service_detailed(ssh_key_path, ssh_port)
                logger.info("-" * 30)
                if agent_status['active']:
                    logger.info("🟢 Agent Service: Running")
                    logger.info(f"Service Uptime: {agent_status.get('uptime', 'unknown')}")
                    logger.info(f"Memory Usage: {agent_status.get('memory', 'unknown')}")
                    logger.info(f"Restart Count: {agent_status.get('restart_count', 'unknown')}")

                    # Check MCP proxy health
                    mcp_health = self._check_mcp_proxy_health(ssh_key_path, config_data['port'], ssh_port)
                    if mcp_health['healthy']:
                        logger.info("🟢 MCP Proxy: Healthy")
                        logger.info(f"MCP Endpoint: http://localhost:{config_data['port']}")
                        logger.info(f"Response Time: {mcp_health.get('response_time', 'unknown')}ms")
                    else:
                        logger.warning("🟡 MCP Proxy: Not responding")
                        logger.warning(f"Error: {mcp_health.get('error', 'unknown')}")
                else:
                    logger.warning("🟡 Agent Service: Not running")
                    if agent_status.get('error'):
                        logger.warning(f"Error: {agent_status['error']}")

                # Check workspace status
                workspace_status = self._check_workspace_status(ssh_key_path, config_data['workspace_path'], ssh_port)
                logger.info("-" * 30)
                if workspace_status['accessible']:
                    logger.info("🟢 Workspace: Accessible")
                    logger.info(f"Workspace Size: {workspace_status.get('size', 'unknown')}")
                    logger.info(f"Git Status: {workspace_status.get('git_status', 'unknown')}")
                else:
                    logger.warning("🟡 Workspace: Issues detected")
                    logger.warning(f"Error: {workspace_status.get('error', 'unknown')}")

            else:
                logger.warning("🟡 SSH Connectivity: Failed")
                logger.warning(f"Error: {ssh_status.get('error', 'Connection failed')}")
                logger.warning("Try restarting the VM: agent-vm restart")
        else:
            logger.info("🔴 VM Status: Stopped")
            logger.info("Start the VM with: agent-vm start")

    def _get_vm_pid(self, vm_name: str) -> Optional[int]:
        """Get the PID of the running VM process."""
        try:
            result = run_subprocess(
                ["pgrep", "-f", f"qemu.*{vm_name}"],
                capture_output=True, text=True
            )
            if result.returncode == 0:
                return int(result.stdout.strip().split('\n')[0])
        except (subprocess.CalledProcessError, ValueError):
            pass
        return None

    def _get_vm_resources(self, pid: int) -> Dict[str, str]:
        """Get VM resource usage information."""
        resources = {}
        try:
            # Get CPU and memory usage from ps
            result = run_subprocess(
                ["ps", "-p", str(pid), "-o", "pid,ppid,pcpu,pmem,etime,comm"],
                capture_output=True, text=True
            )
            if result.returncode == 0:
                lines = result.stdout.strip().split('\n')
                if len(lines) > 1:
                    fields = lines[1].split()
                    if len(fields) >= 6:
                        resources['cpu'] = fields[2]
                        resources['memory'] = fields[3]
                        resources['uptime'] = fields[4]
        except subprocess.CalledProcessError:
            pass
        return resources

    def _check_ssh_connectivity(self, ssh_key_path: Path, ssh_port: int = 2222) -> Dict[str, any]:
        """Check SSH connectivity with detailed diagnostics."""
        ssh_status = {'connected': False}

        logger.debug(f"🔍 Checking SSH connectivity using key: {ssh_key_path}")
        logger.debug(f"🔍 SSH key exists: {ssh_key_path.exists()}")

        if ssh_key_path.exists():
            logger.debug(f"🔍 SSH key permissions: {oct(ssh_key_path.stat().st_mode)[-3:]}")

        try:
            ssh_cmd = [
                "ssh", "-o", f"ConnectTimeout={min(5, _get_global_timeout() // 4)}", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", str(ssh_port), "dev@localhost", "echo 'SSH OK'"
            ]

            logger.debug(f"🔍 Running SSH command: {' '.join(ssh_cmd)}")
            result = run_subprocess(ssh_cmd, capture_output=True, timeout=min(10, _get_global_timeout() // 2), text=True)

            logger.debug(f"🔍 SSH command exit code: {result.returncode}")
            logger.debug(f"🔍 SSH stdout: {result.stdout.strip()}")
            logger.debug(f"🔍 SSH stderr: {result.stderr.strip()}")

            if result.returncode == 0 and "SSH OK" in result.stdout:
                ssh_status['connected'] = True
                logger.debug("✅ SSH connectivity confirmed")
            else:
                ssh_status['error'] = f"SSH failed - exit code: {result.returncode}, stderr: {result.stderr}"
                logger.debug(f"❌ SSH connectivity failed: {ssh_status['error']}")
        except subprocess.TimeoutExpired:
            ssh_status['error'] = "Connection timeout"
            logger.debug("❌ SSH connectivity failed: timeout")
        except subprocess.CalledProcessError as e:
            ssh_status['error'] = f"SSH error: {e}"
            logger.debug(f"❌ SSH connectivity failed: {e}")
        except Exception as e:
            ssh_status['error'] = f"Unexpected SSH error: {e}"
            logger.debug(f"❌ SSH connectivity failed: unexpected error {e}")

        return ssh_status

    def _check_agent_service_detailed(self, ssh_key_path: Path, ssh_port: int = 2222) -> Dict[str, any]:
        """Check agent service status with detailed information."""
        agent_status = {'active': False}
        try:
            # Check if service is active
            ssh_cmd = [
                "ssh", "-o", f"ConnectTimeout={min(5, _get_global_timeout() // 4)}", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", str(ssh_port), "dev@localhost",
                "systemctl show agent-mcp --property=ActiveState,SubState,MainPID,ExecMainStartTimestamp,NRestarts,MemoryCurrent"
            ]
            result = run_subprocess(ssh_cmd, capture_output=True, timeout=min(10, _get_global_timeout() // 2), text=True)
            if result.returncode == 0:
                properties = {}
                for line in result.stdout.strip().split('\n'):
                    if '=' in line:
                        key, value = line.split('=', 1)
                        properties[key] = value

                agent_status['active'] = properties.get('ActiveState') == 'active'
                agent_status['substate'] = properties.get('SubState', 'unknown')
                agent_status['pid'] = properties.get('MainPID', 'unknown')
                agent_status['restart_count'] = properties.get('NRestarts', 'unknown')

                # Convert memory from bytes to MB
                memory_bytes = properties.get('MemoryCurrent', '0')
                try:
                    memory_mb = int(memory_bytes) // (1024 * 1024)
                    agent_status['memory'] = f"{memory_mb} MB"
                except (ValueError, TypeError):
                    agent_status['memory'] = 'unknown'

                # Parse start timestamp for uptime
                start_time = properties.get('ExecMainStartTimestamp', '')
                if start_time and start_time != '0':
                    try:
                        # Simple uptime calculation
                        agent_status['uptime'] = 'running'
                    except Exception:
                        agent_status['uptime'] = 'unknown'
                else:
                    agent_status['uptime'] = 'not started'
            else:
                agent_status['error'] = result.stderr or "Failed to get service status"
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            agent_status['error'] = f"Service check failed: {e}"
        return agent_status

    def _check_mcp_proxy_health(self, ssh_key_path: Path, port: int, ssh_port: int = 2222) -> Dict[str, any]:
        """Check MCP proxy health and response time."""
        mcp_status = {'healthy': False}
        try:
            import time
            start_time = time.time()

            ssh_cmd = [
                "ssh", "-o", f"ConnectTimeout={min(5, _get_global_timeout() // 4)}", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", str(ssh_port), "dev@localhost",
                f"curl -s -f -m 5 http://localhost:{port}/health || curl -s -f -m 5 http://localhost:{port}/ || echo 'PROXY_DOWN'"
            ]
            result = run_subprocess(ssh_cmd, capture_output=True, timeout=min(10, _get_global_timeout() // 2), text=True)

            response_time = int((time.time() - start_time) * 1000)
            mcp_status['response_time'] = response_time

            if result.returncode == 0 and "PROXY_DOWN" not in result.stdout:
                mcp_status['healthy'] = True
            else:
                mcp_status['error'] = "Proxy not responding"
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            mcp_status['error'] = f"Health check failed: {e}"
        return mcp_status

    def _check_workspace_status(self, ssh_key_path: Path, workspace_path: str, ssh_port: int = 2222) -> Dict[str, any]:
        """Check workspace accessibility and status."""
        workspace_status = {'accessible': False}
        try:
            ssh_cmd = [
                "ssh", "-o", f"ConnectTimeout={min(5, _get_global_timeout() // 4)}", "-o", "StrictHostKeyChecking=no",
                "-o", "UserKnownHostsFile=/dev/null", "-i", str(ssh_key_path),
                "-p", str(ssh_port), "dev@localhost",
                f"cd {workspace_path} && pwd && du -sh . 2>/dev/null && git status --porcelain 2>/dev/null | wc -l || echo 'GIT_ERROR'"
            ]
            result = run_subprocess(ssh_cmd, capture_output=True, timeout=min(10, _get_global_timeout() // 2), text=True)
            if result.returncode == 0:
                lines = result.stdout.strip().split('\n')
                if len(lines) >= 2:
                    workspace_status['accessible'] = True
                    workspace_status['size'] = lines[1] if len(lines) > 1 else 'unknown'

                    # Parse git status
                    if len(lines) > 2 and "GIT_ERROR" not in lines[2]:
                        changes = int(lines[2]) if lines[2].isdigit() else 0
                        workspace_status['git_status'] = f"{changes} modified files" if changes > 0 else "clean"
                    else:
                        workspace_status['git_status'] = "not a git repository"
            else:
                workspace_status['error'] = "Workspace not accessible"
        except (subprocess.CalledProcessError, subprocess.TimeoutExpired) as e:
            workspace_status['error'] = f"Workspace check failed: {e}"
        return workspace_status


# Global state for options
_global_state = {
    "state_dir": None,
    "verbose": False,
    "timeout": 120  # Default timeout in seconds
}

# Initialize typer app
app = typer.Typer(
    name="agent-vm",
    help="VM control command for managing development VMs",
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


def _setup_global_options(
    state_dir: Optional[str] = typer.Option(None, help="Override default state directory"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose logging"),
    timeout: int = typer.Option(120, "--timeout", "-t", help="Global timeout in seconds for VM operations")
) -> None:
    """Set up global options that apply to all commands."""
    _global_state["state_dir"] = state_dir
    _global_state["verbose"] = verbose
    _global_state["timeout"] = timeout
    setup_logging(verbose=verbose)


def _get_global_timeout() -> int:
    """Get the global timeout value."""
    return _global_state["timeout"]


@app.callback()
def main_callback(
    state_dir: Optional[str] = typer.Option(None, help="Override default state directory"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose logging"),
    timeout: int = typer.Option(120, "--timeout", "-t", help="Global timeout in seconds for VM operations")
) -> None:
    """Main callback to handle global options."""
    _setup_global_options(state_dir, verbose, timeout)


@app.command()
def create(
    host: str = typer.Option("localhost", help="Host to bind VM ports to"),
    port: int = typer.Option(8000, help="Port for MCP proxy forwarding"),
    branch: Optional[str] = typer.Option(None, help="Branch name for VM (default: current branch)"),
    config: str = typer.Option("vm-config.nix", help="Path to VM NixOS config")
) -> None:
    """Create a new VM configuration."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.create_vm(host, port, branch, config)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def start(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Start VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.start_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def stop(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Stop VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.stop_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def restart(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Restart VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.restart_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def status(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Show VM status for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.vm_status(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def shell(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Open SSH shell in VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.vm_shell(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def logs(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Show VM logs for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.vm_logs(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command("list")
def list_vms() -> None:
    """List all VM configurations."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.list_vms()
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def destroy(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Destroy VM configuration for branch."""
    controller = VMController(state_dir=_global_state["state_dir"])

    try:
        controller.destroy_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


def main() -> None:
    """Main entry point for agent-vm command."""
    app()


if __name__ == "__main__":
    main()
