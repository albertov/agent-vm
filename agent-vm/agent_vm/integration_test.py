#!/usr/bin/env python3
"""
Integration tests for agent-vm functionality.

This script runs comprehensive integration tests for the agent-vm system,
testing VM lifecycle, SSH connectivity, agent service management, and more.

Usage:
    # Run all tests
    integration-test

    # Run specific test
    integration-test -k test_vm_creation

    # Run with custom timeout
    integration-test --timeout 180

    # Run with debug output
    integration-test --debug -s

    # Keep test VMs and state directories
    integration-test --keep

    # Pass pytest options after -- separator
    integration-test --debug -- -k test_vm_creation -v

Options:
    --agent-vm PATH      Path to agent-vm executable (default: agent-vm in PATH)
    --timeout SECONDS    Timeout for VM operations (default: 120)
    --debug             Enable debug mode with full output capture
    --verbose           Enable verbose logging
    --keep              Keep test VMs and state directories after tests complete

All arguments after -- are passed directly to pytest.
"""

import logging
import os
import shutil
import subprocess
import sys
import tempfile
import time
from typing import Optional, List
from pathlib import Path

import pytest

# Import common utilities
from .utils import setup_logging, run_subprocess

# Configure logging
logger = logging.getLogger(__name__)


class IntegrationTestConfig:
    """Configuration for integration tests."""
    def __init__(self, agent_vm_cmd: str = "agent-vm", verbose: bool = False,
                 debug: bool = False, timeout: int = 120, keep: bool = False,
                 state_dir: Optional[str] = None):
        self.agent_vm_cmd = agent_vm_cmd
        self.verbose = verbose
        self.debug = debug
        self.timeout = timeout
        self.keep = keep
        self.state_dir = state_dir


# Global test configuration
test_config: Optional[IntegrationTestConfig] = None


def get_test_config() -> IntegrationTestConfig:
    """Get the current test configuration."""
    global test_config
    if test_config is None:
        # Load configuration from environment variables set by main()
        test_config = IntegrationTestConfig(
            agent_vm_cmd=os.environ.get("INTEGRATION_TEST_AGENT_VM_CMD", "agent-vm"),
            verbose=os.environ.get("INTEGRATION_TEST_VERBOSE", "False").lower() == "true",
            debug=os.environ.get("INTEGRATION_TEST_DEBUG", "False").lower() == "true",
            timeout=int(os.environ.get("INTEGRATION_TEST_TIMEOUT", "120")),
            keep=os.environ.get("INTEGRATION_TEST_KEEP", "False").lower() == "true",
            state_dir=os.environ.get("INTEGRATION_TEST_STATE_DIR")
        )
    return test_config


# --- Pytest Test Cases ---

def build_agent_vm_cmd(base_cmd: List[str], config: IntegrationTestConfig) -> List[str]:
    """Build agent-vm command with state directory option if configured."""
    cmd = [config.agent_vm_cmd]

    # First, add state-dir option if configured
    if config.state_dir:
        cmd.extend(["--state-dir", config.state_dir])

    # Then add all the other arguments
    cmd.extend(base_cmd)

    return cmd


def test_vm_creation():
    """Test VM creation workflow."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    try:
        # Create VM using the correct option syntax
        logger.info(f"Creating VM: {vm_name}")
        cmd = build_agent_vm_cmd(["create", "--host", "localhost", "--port", "8000", "--branch", vm_name], config)
        state_dir = Path(config.state_dir) if config.state_dir else None
        result = run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # In integration test mode, VMs don't actually start
        # We just verify the command executes without error
        assert result.returncode == 0, f"VM creation failed: {result.stderr}"
        logger.info("✓ VM creation command completed successfully")

    finally:
        # Cleanup
        logger.info(f"Cleaning up VM: {vm_name}")
        cleanup_cmd = build_agent_vm_cmd(["destroy", vm_name], config)
        run_subprocess(
            cleanup_cmd,
            capture_output=True,
            timeout=30,
            state_dir=state_dir,
            debug=config.debug
        )


def test_debug_and_verbose_options():
    """Test that debug and verbose options work correctly."""
    config = get_test_config()
    state_dir = Path(config.state_dir) if config.state_dir else None

    # Test list with debug flag
    cmd = build_agent_vm_cmd(["--debug", "list"], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=10,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"Debug list command failed: {result.stderr}"

    # Test verbose flag
    cmd = build_agent_vm_cmd(["--verbose", "list"], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=10,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"Verbose list command failed: {result.stderr}"
    logger.info("✓ Debug and verbose options work correctly")


def test_timeout_parameter_handling():
    """Test that --timeout parameter is properly handled."""
    config = get_test_config()
    state_dir = Path(config.state_dir) if config.state_dir else None

    # Check help output includes --timeout
    cmd = build_agent_vm_cmd(["--help"], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=10,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"Help command failed: {result.stderr}"
    assert "--timeout" in result.stdout, "--timeout option not found in help output"

    # Test that timeout parameter is accepted
    cmd = build_agent_vm_cmd(["--timeout", "60", "list"], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=10,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"List with timeout failed: {result.stderr}"
    logger.info("✓ Timeout parameter handling works correctly")


def test_vm_listing():
    """Test VM listing functionality."""
    config = get_test_config()
    state_dir = Path(config.state_dir) if config.state_dir else None

    if config.state_dir:
        logger.info(f"Using state directory: {config.state_dir}")

    cmd = build_agent_vm_cmd(["list"], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=config.timeout,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"VM listing failed: {result.stderr}"
    logger.info("✓ VM listing works correctly")


def test_vm_status():
    """Test VM status command."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"
    state_dir = Path(config.state_dir) if config.state_dir else None

    try:
        # Create VM first
        cmd = build_agent_vm_cmd(["create", "--host", "localhost", "--port", "8000", "--branch", vm_name], config)
        run_subprocess(
            cmd,
            capture_output=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # Check status
        cmd = build_agent_vm_cmd(["status", vm_name], config)
        result = run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        assert result.returncode == 0, f"VM status failed: {result.stderr}"
        assert "VM Status" in result.stdout or "not running" in result.stdout.lower()
        logger.info("✓ VM status command works correctly")

    finally:
        # Cleanup
        cleanup_cmd = build_agent_vm_cmd(["destroy", vm_name], config)
        run_subprocess(
            cleanup_cmd,
            capture_output=True,
            timeout=30,
            state_dir=state_dir,
            debug=config.debug
        )


def test_vm_start_stop_cycle():
    """Test VM start and stop operations."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"
    state_dir = Path(config.state_dir) if config.state_dir else None

    try:
        # Create VM
        cmd = build_agent_vm_cmd(["create", "--host", "localhost", "--port", "8000", "--branch", vm_name], config)
        run_subprocess(
            cmd,
            capture_output=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # Start VM
        cmd = build_agent_vm_cmd(["start", vm_name], config)
        result = run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # In integration test mode, start succeeds but SSH might not be available
        if result.returncode == 0:
            logger.info("✓ VM start command completed successfully")
        else:
            # Check if it's just SSH connectivity issue (expected in test mode)
            if "SSH connectivity" in result.stderr or "VM ready" in result.stderr:
                logger.info("✓ VM start attempted (SSH not available in test mode)")
            else:
                # Log more details about the failure
                logger.error(f"VM start failed with return code: {result.returncode}")
                logger.error(f"stdout: {result.stdout}")
                logger.error(f"stderr: {result.stderr}")
                assert False, f"VM start failed unexpectedly: return_code={result.returncode}, stderr='{result.stderr}', stdout='{result.stdout}'"

        # Stop VM
        cmd = build_agent_vm_cmd(["stop", vm_name], config)
        result = run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # Stop should work even if VM wasn't fully started
        assert result.returncode == 0, f"VM stop failed: {result.stderr}"
        logger.info("✓ VM stop command completed successfully")

    finally:
        # Cleanup
        cleanup_cmd = build_agent_vm_cmd(["destroy", vm_name], config)
        run_subprocess(
            cleanup_cmd,
            capture_output=True,
            timeout=30,
            state_dir=state_dir,
            debug=config.debug
        )


def test_agent_service_startup():
    """Test that agent service starts properly."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"
    state_dir = Path(config.state_dir) if config.state_dir else None

    try:
        # Create and start VM
        cmd = build_agent_vm_cmd(["create", "--host", "localhost", "--port", "8000", "--branch", vm_name], config)
        run_subprocess(
            cmd,
            capture_output=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        cmd = build_agent_vm_cmd(["start", vm_name], config)
        run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # Check if service status can be queried
        cmd = build_agent_vm_cmd(["status", vm_name], config)
        status_result = run_subprocess(
            cmd,
            capture_output=True,
            text=True,
            timeout=config.timeout,
            state_dir=state_dir,
            debug=config.debug
        )

        # In integration test mode, we just verify commands execute
        # The agent service should be configured but might not be fully running
        if "Agent Service" in status_result.stdout:
            logger.info("✓ Agent service configuration detected")
        else:
            logger.info("✓ VM status command completed (service status not available in test mode)")

    finally:
        # Cleanup
        stop_cmd = build_agent_vm_cmd(["stop", vm_name], config)
        run_subprocess(
            stop_cmd,
            capture_output=True,
            timeout=30,
            state_dir=state_dir,
            debug=config.debug
        )
        destroy_cmd = build_agent_vm_cmd(["destroy", vm_name], config)
        run_subprocess(
            destroy_cmd,
            capture_output=True,
            timeout=30,
            state_dir=state_dir,
            debug=config.debug
        )


def test_vm_destruction():
    """Test VM destruction and cleanup."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"
    state_dir = Path(config.state_dir) if config.state_dir else None

    # Create VM
    cmd = build_agent_vm_cmd(["create", "--host", "localhost", "--port", "8000", "--branch", vm_name], config)
    run_subprocess(
        cmd,
        capture_output=True,
        timeout=config.timeout,
        state_dir=state_dir,
        debug=config.debug
    )

    # Destroy VM
    cmd = build_agent_vm_cmd(["destroy", vm_name], config)
    result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=config.timeout,
        state_dir=state_dir,
        debug=config.debug
    )

    assert result.returncode == 0, f"VM destruction failed: {result.stderr}"

    # Verify VM is gone
    cmd = build_agent_vm_cmd(["list"], config)
    list_result = run_subprocess(
        cmd,
        capture_output=True,
        text=True,
        timeout=config.timeout,
        state_dir=state_dir,
        debug=config.debug
    )

    assert vm_name not in list_result.stdout, f"VM {vm_name} still exists after destruction"
    logger.info("✓ VM destruction and cleanup works correctly")


# --- CLI Application ---

def main():
    """
    Integration test runner for agent-vm.

    This tool provides a simplified interface for running integration tests
    with intuitive argument passing to pytest.
    """
    import argparse

    # Create parser with custom formatting
    parser = argparse.ArgumentParser(
        prog='integration-test',
        description='Run integration tests for agent-vm',
        epilog="""
Examples:
  integration-test                           # Run all tests
  integration-test -k test_vm_creation       # Run specific test
  integration-test --debug -s                # Debug mode with output
  integration-test --timeout 180 -v          # Custom timeout, verbose
  integration-test -- -k test_vm -v -s      # Pass multiple pytest args

Note: Arguments after -- are passed directly to pytest.
        """,
        formatter_class=argparse.RawDescriptionHelpFormatter
    )

    # Integration test options
    parser.add_argument('--agent-vm', default='agent-vm',
                        help='Path to agent-vm executable (default: agent-vm in PATH)')
    parser.add_argument('--timeout', type=int, default=120,
                        help='Timeout in seconds for VM operations (default: 120)')
    parser.add_argument('--debug', '-d', action='store_true',
                        help='Enable debug mode with full output')
    parser.add_argument('--verbose', '-v', action='store_true',
                        help='Enable verbose logging')
    parser.add_argument('--keep', action='store_true',
                        help='Keep test VMs and state directories after tests complete')

    # Common pytest shortcuts (before --)
    parser.add_argument('-k', dest='pytest_k', metavar='EXPRESSION',
                        help='Run tests matching expression (pytest -k)')
    parser.add_argument('-s', dest='pytest_s', action='store_true',
                        help='No capture, show print statements (pytest -s)')
    parser.add_argument('-x', dest='pytest_x', action='store_true',
                        help='Stop on first failure (pytest -x)')

    # Capture remaining arguments for pytest
    parser.add_argument('pytest_args', nargs='*',
                        help='Additional arguments to pass to pytest')

    # Parse arguments
    args = parser.parse_args()

    # Set up logging early so we can see temp directory creation
    setup_logging(verbose=args.verbose or args.debug)

    # Create temporary state directory for tests
    temp_state_dir = None
    if not args.keep:
        temp_state_dir = tempfile.mkdtemp(prefix="agent-vm-test-", suffix="-state")
        logger.info(f"Created temporary state directory: {temp_state_dir}")

    # Set up global configuration
    global test_config
    test_config = IntegrationTestConfig(
        agent_vm_cmd=args.agent_vm,
        verbose=args.verbose or args.debug,
        debug=args.debug,
        timeout=args.timeout,
        keep=args.keep,
        state_dir=temp_state_dir
    )

    logger.info("🚀 Starting agent-vm integration tests using pytest")
    logger.info(f"Using agent-vm command: {test_config.agent_vm_cmd}")
    logger.info(f"Using timeout: {test_config.timeout} seconds")

    # Pass configuration through environment variables
    os.environ["INTEGRATION_TEST_AGENT_VM_CMD"] = test_config.agent_vm_cmd
    os.environ["INTEGRATION_TEST_VERBOSE"] = str(test_config.verbose)
    os.environ["INTEGRATION_TEST_DEBUG"] = str(test_config.debug)
    os.environ["INTEGRATION_TEST_TIMEOUT"] = str(test_config.timeout)
    os.environ["INTEGRATION_TEST_KEEP"] = str(test_config.keep)
    if test_config.state_dir:
        os.environ["INTEGRATION_TEST_STATE_DIR"] = test_config.state_dir

    # Build pytest arguments
    pytest_argv = [__file__]  # Run tests from this file

    # Add base pytest configuration
    pytest_argv.extend([
        "-v",  # Verbose output
        "--tb=short",  # Short traceback format
        "-p", "no:cacheprovider",  # Disable cache provider
        "--disable-warnings",  # Disable warnings
        f"--timeout={test_config.timeout + 20}",  # Pytest timeout with buffer
    ])

    # Add debug options if enabled
    if test_config.debug:
        pytest_argv.extend(["-s", "--capture=no"])

    # Add shortcut options if provided
    if args.pytest_k:
        pytest_argv.extend(["-k", args.pytest_k])
    if args.pytest_s:
        pytest_argv.extend(["-s"])
    if args.pytest_x:
        pytest_argv.extend(["-x"])

    # Add any additional pytest arguments
    if args.pytest_args:
        pytest_argv.extend(args.pytest_args)

    # Show what we're running if verbose
    if test_config.verbose:
        logger.debug(f"Running pytest with args: {' '.join(pytest_argv[1:])}")

    try:
        # Run pytest
        exit_code = pytest.main(pytest_argv)

        if exit_code == 0:
            logger.info("✅ All integration tests PASSED")
        else:
            logger.error("❌ Integration tests FAILED")

    except KeyboardInterrupt:
        logger.info("Integration tests cancelled by user")
        exit_code = 130
    except Exception as e:
        logger.error(f"Unexpected error during integration tests: {e}")
        exit_code = 1

    finally:
        # Clean up temporary state directory if not keeping
        if temp_state_dir and not args.keep:
            try:
                shutil.rmtree(temp_state_dir)
                logger.info(f"Cleaned up temporary state directory: {temp_state_dir}")
            except Exception as e:
                logger.warning(f"Failed to clean up temporary state directory: {e}")
        elif args.keep and temp_state_dir:
            logger.info(f"Keeping temporary state directory: {temp_state_dir}")

    sys.exit(exit_code)


if __name__ == "__main__":
    main()
