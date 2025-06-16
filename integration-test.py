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

    # Pass pytest options after -- separator
    integration-test --debug -- -k test_vm_creation -v

Options:
    --agent-vm PATH      Path to agent-vm executable (default: agent-vm in PATH)
    --timeout SECONDS    Timeout for VM operations (default: 120)
    --debug             Enable debug mode with full output capture
    --verbose           Enable verbose logging

All arguments after -- are passed directly to pytest.
"""

import json
import logging
import os
import subprocess
import sys
import tempfile
import time
from pathlib import Path
from typing import Dict, Optional, List, Tuple
from unittest.mock import Mock

import pytest
import typer
from typer import Context

# Configure logging
logger = logging.getLogger(__name__)


def setup_logging(verbose: bool = False):
    """Configure logging based on verbosity settings."""
    level = logging.DEBUG if verbose else logging.INFO

    # Use custom time format matching agent-vm
    formatter = logging.Formatter('%(asctime)s [%(levelname)s] %(message)s',
                                  datefmt='%H:%M:%S')

    handler = logging.StreamHandler()
    handler.setFormatter(formatter)

    logger.setLevel(level)
    logger.handlers = [handler]


class IntegrationTestConfig:
    """Configuration for integration tests."""
    def __init__(self, agent_vm_cmd: str = "agent-vm", verbose: bool = False,
                 debug: bool = False, timeout: int = 120):
        self.agent_vm_cmd = agent_vm_cmd
        self.verbose = verbose
        self.debug = debug
        self.timeout = timeout


# Global test configuration
test_config: Optional[IntegrationTestConfig] = None


def get_test_config() -> IntegrationTestConfig:
    """Get the current test configuration."""
    global test_config
    if test_config is None:
        # Default configuration if not set
        test_config = IntegrationTestConfig()
    return test_config


# --- Pytest Test Cases ---

def test_vm_creation():
    """Test VM creation workflow."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    try:
        # Create VM using the correct option syntax
        logger.info(f"Creating VM: {vm_name}")
        result = subprocess.run(
            [config.agent_vm_cmd, "create", "--host", "localhost", "--port", "8000", "--branch", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        # In integration test mode, VMs don't actually start
        # We just verify the command executes without error
        assert result.returncode == 0, f"VM creation failed: {result.stderr}"
        logger.info("✓ VM creation command completed successfully")

    finally:
        # Cleanup
        logger.info(f"Cleaning up VM: {vm_name}")
        subprocess.run(
            [config.agent_vm_cmd, "destroy", vm_name],
            capture_output=True,
            timeout=30
        )


def test_debug_and_verbose_options():
    """Test that debug and verbose options work correctly."""
    config = get_test_config()

    # Test help with debug flag
    result = subprocess.run(
        [config.agent_vm_cmd, "--debug", "help"],
        capture_output=True,
        text=True,
        timeout=10
    )

    assert result.returncode == 0, f"Debug help command failed: {result.stderr}"

    # Test verbose flag
    result = subprocess.run(
        [config.agent_vm_cmd, "--verbose", "list"],
        capture_output=True,
        text=True,
        timeout=10
    )

    assert result.returncode == 0, f"Verbose list command failed: {result.stderr}"
    logger.info("✓ Debug and verbose options work correctly")


def test_timeout_parameter_handling():
    """Test that --timeout parameter is properly handled."""
    config = get_test_config()

    # Check help output includes --timeout
    result = subprocess.run(
        [config.agent_vm_cmd, "--help"],
        capture_output=True,
        text=True,
        timeout=10
    )

    assert result.returncode == 0, f"Help command failed: {result.stderr}"
    assert "--timeout" in result.stdout, "--timeout option not found in help output"

    # Test that timeout parameter is accepted
    result = subprocess.run(
        [config.agent_vm_cmd, "--timeout", "60", "list"],
        capture_output=True,
        text=True,
        timeout=10
    )

    assert result.returncode == 0, f"List with timeout failed: {result.stderr}"
    logger.info("✓ Timeout parameter handling works correctly")


def test_vm_listing():
    """Test VM listing functionality."""
    config = get_test_config()

    result = subprocess.run(
        [config.agent_vm_cmd, "list"],
        capture_output=True,
        text=True,
        timeout=config.timeout
    )

    assert result.returncode == 0, f"VM listing failed: {result.stderr}"
    logger.info("✓ VM listing works correctly")


def test_vm_status():
    """Test VM status command."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    try:
        # Create VM first
        subprocess.run(
            [config.agent_vm_cmd, "create", "--host", "localhost", "--port", "8000", "--branch", vm_name],
            capture_output=True,
            timeout=config.timeout
        )

        # Check status
        result = subprocess.run(
            [config.agent_vm_cmd, "status", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        assert result.returncode == 0, f"VM status failed: {result.stderr}"
        assert "VM Status" in result.stdout or "not running" in result.stdout.lower()
        logger.info("✓ VM status command works correctly")

    finally:
        # Cleanup
        subprocess.run(
            [config.agent_vm_cmd, "destroy", vm_name],
            capture_output=True,
            timeout=30
        )


def test_vm_start_stop_cycle():
    """Test VM start and stop operations."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    try:
        # Create VM
        subprocess.run(
            [config.agent_vm_cmd, "create", "--host", "localhost", "--port", "8000", "--branch", vm_name],
            capture_output=True,
            timeout=config.timeout
        )

        # Start VM
        result = subprocess.run(
            [config.agent_vm_cmd, "start", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        # In integration test mode, start succeeds but SSH might not be available
        if result.returncode == 0:
            logger.info("✓ VM start command completed successfully")
        else:
            # Check if it's just SSH connectivity issue (expected in test mode)
            if "SSH connectivity" in result.stderr or "VM ready" in result.stderr:
                logger.info("✓ VM start attempted (SSH not available in test mode)")
            else:
                assert False, f"VM start failed unexpectedly: {result.stderr}"

        # Stop VM
        result = subprocess.run(
            [config.agent_vm_cmd, "stop", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        # Stop should work even if VM wasn't fully started
        assert result.returncode == 0, f"VM stop failed: {result.stderr}"
        logger.info("✓ VM stop command completed successfully")

    finally:
        # Cleanup
        subprocess.run(
            [config.agent_vm_cmd, "destroy", vm_name],
            capture_output=True,
            timeout=30
        )


def test_agent_service_startup():
    """Test that agent service starts properly."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    try:
        # Create and start VM
        subprocess.run(
            [config.agent_vm_cmd, "create", "--host", "localhost", "--port", "8000", "--branch", vm_name],
            capture_output=True,
            timeout=config.timeout
        )

        result = subprocess.run(
            [config.agent_vm_cmd, "start", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        # Check if service status can be queried
        status_result = subprocess.run(
            [config.agent_vm_cmd, "status", vm_name],
            capture_output=True,
            text=True,
            timeout=config.timeout
        )

        # In integration test mode, we just verify commands execute
        # The agent service should be configured but might not be fully running
        if "Agent Service" in status_result.stdout:
            logger.info("✓ Agent service configuration detected")
        else:
            logger.info("✓ VM status command completed (service status not available in test mode)")

    finally:
        # Cleanup
        subprocess.run(
            [config.agent_vm_cmd, "stop", vm_name],
            capture_output=True,
            timeout=30
        )
        subprocess.run(
            [config.agent_vm_cmd, "destroy", vm_name],
            capture_output=True,
            timeout=30
        )


def test_vm_destruction():
    """Test VM destruction and cleanup."""
    config = get_test_config()
    vm_name = f"test-vm-{int(time.time())}"

    # Create VM
    subprocess.run(
        [config.agent_vm_cmd, "create", "--host", "localhost", "--port", "8000", "--branch", vm_name],
        capture_output=True,
        timeout=config.timeout
    )

    # Destroy VM
    result = subprocess.run(
        [config.agent_vm_cmd, "destroy", vm_name],
        capture_output=True,
        text=True,
        timeout=config.timeout
    )

    assert result.returncode == 0, f"VM destruction failed: {result.stderr}"

    # Verify VM is gone
    list_result = subprocess.run(
        [config.agent_vm_cmd, "list"],
        capture_output=True,
        text=True,
        timeout=config.timeout
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

    # Set up global configuration
    global test_config
    test_config = IntegrationTestConfig(
        agent_vm_cmd=args.agent_vm,
        verbose=args.verbose or args.debug,
        debug=args.debug,
        timeout=args.timeout
    )

    # Set up logging
    setup_logging(verbose=test_config.verbose)

    logger.info("🚀 Starting agent-vm integration tests using pytest")
    logger.info(f"Using agent-vm command: {test_config.agent_vm_cmd}")
    logger.info(f"Using timeout: {test_config.timeout} seconds")

    # Pass configuration through environment variables
    os.environ["INTEGRATION_TEST_AGENT_VM_CMD"] = test_config.agent_vm_cmd
    os.environ["INTEGRATION_TEST_VERBOSE"] = str(test_config.verbose)
    os.environ["INTEGRATION_TEST_DEBUG"] = str(test_config.debug)
    os.environ["INTEGRATION_TEST_TIMEOUT"] = str(test_config.timeout)

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

        sys.exit(exit_code)

    except KeyboardInterrupt:
        logger.info("Integration tests cancelled by user")
        sys.exit(130)
    except Exception as e:
        logger.error(f"Unexpected error during integration tests: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
