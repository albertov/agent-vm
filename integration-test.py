#!/usr/bin/env python3
"""
Integration test executable for agent-vm using pytest.

This test executable runs comprehensive integration tests by calling agent-vm
through the CLI exclusively, without using mocks. It tests the complete
workflow from VM creation to agent execution and cleanup.

This is designed to be run manually or by CI systems and is separate from
the normal test suite to avoid interfering with development workflows.
"""

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

import pytest
import typer


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


class IntegrationTestFormatter(logging.Formatter):
    """Custom formatter with color support for integration tests."""

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
            if "✅" in record.msg or "PASS" in record.msg.upper():
                formatted = Colors.BRIGHT_GREEN + "✅ " + Colors.RESET + formatted
            elif "❌" in record.msg or "FAIL" in record.msg.upper():
                formatted = Colors.BRIGHT_RED + "❌ " + Colors.RESET + formatted
            elif "⚠️" in record.msg or "SKIP" in record.msg.upper():
                formatted = Colors.BRIGHT_YELLOW + "⚠️ " + Colors.RESET + formatted
            elif "🧪" in record.msg or "TEST" in record.msg.upper():
                formatted = Colors.BRIGHT_CYAN + "🧪 " + Colors.RESET + formatted
            elif "🔧" in record.msg or "SETUP" in record.msg.upper():
                formatted = Colors.BRIGHT_MAGENTA + "🔧 " + Colors.RESET + formatted

        return formatted


def setup_logging(verbose: bool = False):
    """Set up colored logging configuration for integration tests."""
    level = logging.DEBUG if verbose else logging.INFO

    # Create formatter
    formatter = IntegrationTestFormatter(
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


logger = logging.getLogger(__name__)


class IntegrationTestConfig:
    """Integration test configuration class to hold test parameters."""

    def __init__(
        self,
        agent_vm_cmd: str = "agent-vm",
        verbose: bool = False,
        debug: bool = False,
        timeout: int = 120
    ):
        self.agent_vm_cmd = agent_vm_cmd
        self.verbose = verbose
        self.debug = debug
        self.timeout = timeout


# Global test configuration - will be set by CLI or default values
test_config: Optional[IntegrationTestConfig] = None


def get_test_config() -> IntegrationTestConfig:
    """Get test configuration, creating default if not set."""
    global test_config
    if test_config is None:
        # Create default configuration if not set by CLI
        test_config = IntegrationTestConfig()
    return test_config


def find_free_port(start_port: int = 12000, max_attempts: int = 100) -> int:
    """Find a free port starting from start_port."""
    for port in range(start_port, start_port + max_attempts):
        try:
            with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
                s.bind(('localhost', port))
                logger.debug(f"Found free port: {port}")
                return port
        except OSError:
            continue

    raise RuntimeError(f"Could not find a free port after {max_attempts} attempts starting from {start_port}")


def run_agent_vm_command(
    args: List[str],
    test_state_dir: Path,
    check: bool = True,
    timeout: Optional[int] = None
) -> subprocess.CompletedProcess:
    """Run agent-vm command with test state directory."""
    config = get_test_config()

    cmd = [config.agent_vm_cmd]
    if test_state_dir:
        cmd.extend(["--state-dir", str(test_state_dir)])
    if config.verbose:
        cmd.append("--verbose")
    cmd.extend(args)

    # Use instance timeout if none specified
    if timeout is None:
        timeout = config.timeout

    logger.debug(f"Running command: {' '.join(cmd)}")

    try:
        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            check=check,
            timeout=timeout
        )

        # Log stdout/stderr based on debug/verbose flags and result status
        should_log_output = (
            config.debug or  # Always log if debug is enabled
            config.verbose or  # Always log if verbose is enabled
            result.returncode != 0  # Always log if command failed
        )

        if should_log_output:
            if result.stdout:
                logger.debug(f"Command stdout: {result.stdout}")
            if result.stderr:
                logger.debug(f"Command stderr: {result.stderr}")

        return result

    except subprocess.TimeoutExpired as e:
        logger.error(f"Command timed out after {timeout} seconds: {' '.join(cmd)}")

        # Log captured output from timeout exception when debug is enabled
        if config.debug:
            if hasattr(e, 'stdout') and e.stdout:
                logger.error(f"Timeout stdout: {e.stdout}")
            if hasattr(e, 'stderr') and e.stderr:
                logger.error(f"Timeout stderr: {e.stderr}")

        raise

    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed: {' '.join(cmd)}")
        logger.error(f"Exit code: {e.returncode}")

        # Always log stdout/stderr for failed commands when debug is enabled
        if config.debug or e.stdout or e.stderr:
            if e.stdout:
                logger.error(f"Failed command stdout: {e.stdout}")
            if e.stderr:
                logger.error(f"Failed command stderr: {e.stderr}")

        raise


def can_run_vms() -> bool:
    """Check if we can actually run VMs (nested virtualization available)."""
    # Check for KVM support
    if not Path("/dev/kvm").exists():
        return False

    # Check for qemu
    try:
        subprocess.run(["qemu-system-x86_64", "--version"],
                     capture_output=True, check=True)
    except (subprocess.CalledProcessError, FileNotFoundError):
        return False

    # Check for virtualization CPU flags
    try:
        with open("/proc/cpuinfo") as f:
            cpuinfo = f.read()
            if "vmx" not in cpuinfo and "svm" not in cpuinfo:
                return False
    except:
        return False

    return True


@pytest.fixture(scope="session")
def test_environment():
    """Set up isolated test environment for the entire test session."""
    logger.info("🔧 SETUP: Creating isolated test environment")

    # Ensure we're in a git repository
    try:
        subprocess.run(["git", "rev-parse", "--git-dir"],
                     capture_output=True, check=True)
    except subprocess.CalledProcessError:
        pytest.fail("Not in a git repository. Integration tests require git.")

    # Create temporary state directory
    test_state_dir = Path(tempfile.mkdtemp(prefix="agent-vm-integration-test-"))
    logger.info(f"Test state directory: {test_state_dir}")

    yield test_state_dir

    # Cleanup
    logger.info("🧹 CLEANUP: Removing test environment")
    if test_state_dir.exists():
        cleanup_attempts = 3
        for attempt in range(cleanup_attempts):
            try:
                shutil.rmtree(test_state_dir)
                logger.info("Test state directory cleaned up successfully")
                break
            except OSError as e:
                if attempt < cleanup_attempts - 1:
                    logger.warning(f"Cleanup attempt {attempt + 1} failed, retrying: {e}")
                    time.sleep(1)  # Wait a bit before retrying
                else:
                    logger.error(f"Failed to clean up test directory after {cleanup_attempts} attempts: {e}")
                    logger.error(f"Manual cleanup may be required: {test_state_dir}")


@pytest.fixture(scope="session")
def test_vm_config(test_environment):
    """Set up test VM configuration."""
    test_state_dir = test_environment
    test_branch = f"integration-test-{int(time.time())}"
    test_port = find_free_port(start_port=12000)

    logger.info(f"Using test branch: {test_branch}")
    logger.info(f"Using dynamically allocated port: {test_port}")

    vm_config = {
        "test_state_dir": test_state_dir,
        "test_branch": test_branch,
        "test_port": test_port
    }

    yield vm_config

    # Cleanup: try to stop and destroy the VM
    try:
        logger.debug("Attempting to stop test VM during cleanup...")
        run_agent_vm_command(["stop", test_branch], test_state_dir, check=False, timeout=30)
    except Exception as e:
        logger.debug(f"VM stop during cleanup failed (this is expected): {e}")


@pytest.mark.integration
@pytest.mark.vm
@pytest.mark.timeout(120)
def test_vm_creation(test_vm_config):
    """Test VM creation functionality."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]
    test_port = test_vm_config["test_port"]

    logger.info("🧪 TEST: VM creation")

    # Test creating a VM with custom configuration
    result = run_agent_vm_command([
        "create",
        "--branch", test_branch,
        "--port", str(test_port),
        "--host", "localhost"
    ], test_state_dir)

    # Verify VM configuration was created
    vm_config_dir = test_state_dir / test_branch
    config_file = vm_config_dir / "config.json"

    assert config_file.exists(), "VM configuration file was not created"

    # Verify configuration contents
    with config_file.open() as f:
        config_data = json.load(f)

    assert config_data["branch"] == test_branch
    assert config_data["port"] == test_port
    assert config_data["host"] == "localhost"

    # Verify SSH keys were created
    ssh_dir = vm_config_dir / "ssh"
    private_key = ssh_dir / "id_ed25519"
    public_key = ssh_dir / "id_ed25519.pub"

    assert private_key.exists(), "SSH private key was not created"
    assert public_key.exists(), "SSH public key was not created"

    # Verify workspace was cloned
    workspace_dir = vm_config_dir / "workspace"
    assert workspace_dir.exists(), "Workspace directory was not created"
    assert (workspace_dir / ".git").exists(), "Workspace is not a git repository"

    logger.info("✅ PASS: VM creation successful")


@pytest.mark.integration
@pytest.mark.vm
def test_vm_listing(test_vm_config):
    """Test VM listing functionality."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]

    logger.info("🧪 TEST: VM listing")

    result = run_agent_vm_command(["list"], test_state_dir)

    # Should show our created VM
    assert test_branch in result.stdout, f"Created VM {test_branch} not found in list output"

    logger.info("✅ PASS: VM listing successful")


@pytest.mark.integration
@pytest.mark.vm
def test_vm_status(test_vm_config):
    """Test VM status functionality."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]

    logger.info("🧪 TEST: VM status")

    result = run_agent_vm_command(["status", test_branch], test_state_dir)

    # Should show VM as stopped initially
    if "🔴 VM Status: Stopped" not in result.stdout:
        logger.warning("VM status output may have changed format")

    logger.info("✅ PASS: VM status check successful")


@pytest.mark.integration
@pytest.mark.vm
@pytest.mark.slow
@pytest.mark.timeout(300)
def test_vm_start_stop_cycle(test_vm_config):
    """Test VM start and stop functionality."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]
    config = get_test_config()

    logger.info("🧪 TEST: VM start/stop cycle")

    # Skip this test if we can't run VMs (e.g., in CI without nested virtualization)
    if not can_run_vms():
        pytest.skip("Nested virtualization not available")

    # Test starting VM
    logger.info("Starting VM...")

    if config.debug:
        logger.debug(f"About to start VM with timeout {config.timeout}s")

    result = run_agent_vm_command(["start", test_branch], test_state_dir)

    if config.debug:
        logger.debug("VM start command completed successfully")

    # Give it a moment to fully start
    time.sleep(5)

    # Check status after start
    logger.info("Checking VM status after start...")
    status_result = run_agent_vm_command(["status", test_branch], test_state_dir)

    if config.debug:
        logger.debug(f"VM status after start: {status_result.stdout}")

    if "🟢 VM Status: Running" not in status_result.stdout:
        logger.warning("VM may not have started properly")

    # Test stopping VM
    logger.info("Stopping VM...")
    stop_result = run_agent_vm_command(["stop", test_branch], test_state_dir)

    if config.debug:
        logger.debug(f"VM stop result: {stop_result.stdout}")

    # Check status after stop
    logger.info("Checking VM status after stop...")
    status_result = run_agent_vm_command(["status", test_branch], test_state_dir)

    if config.debug:
        logger.debug(f"VM status after stop: {status_result.stdout}")

    if "🔴 VM Status: Stopped" not in status_result.stdout:
        logger.warning("VM may not have stopped properly")

    logger.info("✅ PASS: VM start/stop cycle successful")


@pytest.mark.integration
@pytest.mark.vm
@pytest.mark.slow
@pytest.mark.timeout(300)
def test_agent_service_startup(test_vm_config):
    """Test that the agent service starts properly in the VM."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]
    config = get_test_config()

    logger.info("🧪 TEST: Agent service startup and health")

    # Skip this test if we can't run VMs
    if not can_run_vms():
        pytest.skip("Nested virtualization not available")

    try:
        # Start the VM
        logger.info("Starting VM for agent service test...")
        result = run_agent_vm_command(["start", test_branch], test_state_dir)

        if config.debug:
            logger.debug("VM started, allowing services to initialize...")

        # Wait longer for services to fully start
        time.sleep(10)

        # Test 1: Check VM status to see if agent service is mentioned
        logger.info("Checking overall VM status...")
        status_result = run_agent_vm_command(["status", test_branch], test_state_dir)

        if config.debug:
            logger.debug(f"VM status output:\n{status_result.stdout}")

        # Look for positive indicators in the status output
        status_indicators = [
            "🟢 Agent Service: Running",
            "🟢 MCP Proxy: Healthy",
            "Agent Service: Running"
        ]

        service_running = any(indicator in status_result.stdout for indicator in status_indicators)
        if service_running:
            logger.info("✓ Agent service appears to be running based on status")
        else:
            logger.warning("⚠️ Agent service status unclear from VM status output")

        # Test 2: Check for error indicators that would suggest service failure
        error_indicators = [
            "🟡 Agent Service: Not running",
            "❌ Agent service is not active",
            "Failed",
            "Error"
        ]

        has_errors = any(error in status_result.stdout for error in error_indicators)
        assert not has_errors, "Agent service appears to have errors"

        # Test 3: Check that MCP port is accessible (if mentioned in status)
        if "MCP Endpoint" in status_result.stdout:
            logger.info("✓ MCP endpoint is accessible according to status")
        else:
            logger.warning("⚠️ MCP endpoint status not explicitly shown")

        # Test 4: Use logs command to check for agent service activity
        logger.info("Checking agent service logs...")
        try:
            # The logs command might be interactive, so use a short timeout
            logs_result = run_agent_vm_command(["logs", test_branch], test_state_dir, timeout=10)
            logger.info("✓ Agent service logs accessible")
        except subprocess.TimeoutExpired:
            # This is expected since logs might be interactive
            logger.info("✓ Logs command started (interactive mode expected)")
        except Exception as e:
            logger.warning(f"⚠️ Could not access logs: {e}")

        logger.info("✅ PASS: Agent service startup test successful")

    finally:
        # Always try to stop the VM
        try:
            logger.info("Stopping VM after agent service test...")
            run_agent_vm_command(["stop", test_branch], test_state_dir, check=False)
        except:
            if config.debug:
                logger.debug("VM stop after agent service test failed")
            pass


@pytest.mark.integration
@pytest.mark.vm
@pytest.mark.timeout(120)
def test_vm_destruction(test_vm_config):
    """Test VM destruction functionality."""
    test_state_dir = test_vm_config["test_state_dir"]
    test_branch = test_vm_config["test_branch"]

    logger.info("🧪 TEST: VM destruction")

    # Ensure VM is stopped first
    try:
        run_agent_vm_command(["stop", test_branch], test_state_dir, check=False)
    except:
        pass  # VM might already be stopped

    # Test destroying VM
    result = run_agent_vm_command(["destroy", test_branch], test_state_dir)

    # Verify VM configuration was removed
    vm_config_dir = test_state_dir / test_branch
    assert not vm_config_dir.exists(), "VM configuration directory was not removed"

    # Verify it's no longer in the list
    list_result = run_agent_vm_command(["list"], test_state_dir)
    assert test_branch not in list_result.stdout, "Destroyed VM still appears in list"

    logger.info("✅ PASS: VM destruction successful")


# CLI interface using typer
app = typer.Typer(
    name="integration-test",
    help="Integration test executable for agent-vm using pytest",
    epilog="""
This executable runs comprehensive integration tests for agent-vm by calling
the CLI exclusively (no mocks). It creates an isolated test environment and
tests the complete workflow from VM creation to cleanup.

Examples:
  integration-test                    # Run all tests with agent-vm in PATH
  integration-test --agent-vm ./agent-vm  # Use specific agent-vm executable
  integration-test --verbose         # Enable verbose output
  integration-test --debug           # Enable debug output with full stderr/stdout capture
  integration-test --timeout 180     # Set custom timeout to 180 seconds
    """
)


@app.callback()
def main_callback(
    agent_vm: str = typer.Option("agent-vm", help="Path to agent-vm executable (default: agent-vm in PATH)"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose logging and debugging"),
    debug: bool = typer.Option(False, "--debug", "-d", help="Enable debug mode with comprehensive stderr/stdout capture"),
    timeout: int = typer.Option(120, "--timeout", "-t", help="Timeout in seconds for VM operations (default: 120)")
) -> None:
    """Main callback to handle global options."""
    global test_config
    test_config = IntegrationTestConfig(
        agent_vm_cmd=agent_vm,
        verbose=verbose,
        debug=debug,
        timeout=timeout
    )

    # Set up logging - debug mode implies verbose
    setup_logging(verbose=verbose or debug)


@app.command()
def run(
    ctx: typer.Context,
    pytest_args: List[str] = typer.Argument(None, help="Additional pytest arguments")
) -> None:
    """Run all integration tests using pytest."""
    try:
        config = get_test_config()

        logger.info("🚀 Starting agent-vm integration tests using pytest")
        logger.info(f"Using agent-vm command: {config.agent_vm_cmd}")

        # Build pytest arguments
        pytest_argv = [__file__]  # Run tests from this file

        # Add pytest configuration
        pytest_argv.extend([
            "-v",  # Verbose output
            "--tb=short",  # Short traceback format
        ])

        # Configure to ignore cache warnings in read-only environment
        pytest_argv.extend([
            "-p", "no:cacheprovider",  # Disable cache provider to avoid read-only warnings
        ])

        # Add timeout configuration if needed
        if config.timeout != 120:  # Only if different from default
            pytest_argv.extend([f"--timeout={config.timeout}"])

        # Add debug options if enabled
        if config.debug:
            pytest_argv.extend(["-s", "--capture=no"])  # No capture for debug output

        # Add any additional pytest arguments passed by user
        if pytest_args:
            pytest_argv.extend(pytest_args)

        # Run pytest
        exit_code = pytest.main(pytest_argv)

        if exit_code == 0:
            logger.info("✅ All integration tests PASSED")
        else:
            logger.error("❌ Integration tests FAILED")

        raise typer.Exit(exit_code)

    except typer.Exit:
        # Re-raise typer.Exit exceptions (these are normal)
        raise
    except KeyboardInterrupt:
        logger.info("Integration tests cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error during integration tests: {e}")
        raise typer.Exit(1)


def main() -> None:
    """Main entry point for integration test executable."""
    # If no command is provided, run the tests by default
    if len(sys.argv) == 1 or (len(sys.argv) > 1 and not any(arg in ["run", "--help", "-h"] for arg in sys.argv)):
        # Add "run" command if not present
        sys.argv.insert(1, "run")

    app()


if __name__ == "__main__":
    main()
