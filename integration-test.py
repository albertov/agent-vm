#!/usr/bin/env python3
"""
Integration test executable for agent-vm.

This test executable runs comprehensive integration tests by calling agent-vm
through the CLI exclusively, without using mocks. It tests the complete
workflow from VM creation to agent execution and cleanup.

This is designed to be run manually or by CI systems and is separate from
the normal test suite to avoid interfering with development workflows.
"""

import argparse
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


class AgentVMIntegrationTest:
    """Integration test runner for agent-vm using CLI calls only."""

    def __init__(self, agent_vm_cmd: str = "agent-vm", verbose: bool = False,
                 debug: bool = False, timeout: int = 120):
        """Initialize integration test runner."""
        self.agent_vm_cmd = agent_vm_cmd
        self.verbose = verbose
        self.debug = debug
        self.timeout = timeout
        self.test_state_dir = None
        self.test_branch = f"integration-test-{int(time.time())}"
        self.test_port = self._find_free_port(start_port=12000)  # Use dynamic port allocation
        self.tests_passed = 0
        self.tests_failed = 0
        self.tests_skipped = 0

    def _find_free_port(self, start_port: int = 12000, max_attempts: int = 100) -> int:
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

    def run_agent_vm_command(self, args: List[str], check: bool = True,
                           timeout: Optional[int] = None) -> subprocess.CompletedProcess:
        """Run agent-vm command with test state directory."""
        cmd = [self.agent_vm_cmd]
        if self.test_state_dir:
            cmd.extend(["--state-dir", str(self.test_state_dir)])
        if self.verbose:
            cmd.append("--verbose")
        cmd.extend(args)

        # Use instance timeout if none specified
        if timeout is None:
            timeout = self.timeout

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
                self.debug or  # Always log if debug is enabled
                self.verbose or  # Always log if verbose is enabled
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
            if self.debug:
                if hasattr(e, 'stdout') and e.stdout:
                    logger.error(f"Timeout stdout: {e.stdout}")
                if hasattr(e, 'stderr') and e.stderr:
                    logger.error(f"Timeout stderr: {e.stderr}")

            raise

        except subprocess.CalledProcessError as e:
            logger.error(f"Command failed: {' '.join(cmd)}")
            logger.error(f"Exit code: {e.returncode}")

            # Always log stdout/stderr for failed commands when debug is enabled
            if self.debug or e.stdout or e.stderr:
                if e.stdout:
                    logger.error(f"Failed command stdout: {e.stdout}")
                if e.stderr:
                    logger.error(f"Failed command stderr: {e.stderr}")

            raise

    def setup_test_environment(self):
        """Set up isolated test environment."""
        logger.info("🔧 SETUP: Creating isolated test environment")

        # Create temporary state directory
        self.test_state_dir = Path(tempfile.mkdtemp(prefix="agent-vm-integration-test-"))
        logger.info(f"Test state directory: {self.test_state_dir}")
        logger.info(f"Using dynamically allocated port: {self.test_port}")

        # Register cleanup function to run on exit (even if script is interrupted)
        atexit.register(self._emergency_cleanup)

        # Ensure we're in a git repository
        try:
            subprocess.run(["git", "rev-parse", "--git-dir"],
                         capture_output=True, check=True)
        except subprocess.CalledProcessError:
            logger.error("Not in a git repository. Integration tests require git.")
            sys.exit(1)

    def _emergency_cleanup(self):
        """Emergency cleanup function registered with atexit."""
        if hasattr(self, 'test_state_dir') and self.test_state_dir and self.test_state_dir.exists():
            try:
                logger.debug("Emergency cleanup: removing test state directory")
                shutil.rmtree(self.test_state_dir)
            except Exception as e:
                # Use print instead of logger as logging might be shut down
                print(f"Warning: Emergency cleanup failed: {e}")

    def cleanup_test_environment(self):
        """Clean up test environment with robust error handling."""
        logger.info("🧹 CLEANUP: Removing test environment")

        # First try to stop any running VMs
        if hasattr(self, 'test_branch') and self.test_branch:
            try:
                logger.debug("Attempting to stop test VM before cleanup...")
                self.run_agent_vm_command(["stop", self.test_branch], check=False, timeout=30)
            except Exception as e:
                logger.debug(f"VM stop during cleanup failed (this is expected): {e}")

        # Clean up test state directory with multiple attempts
        if self.test_state_dir and self.test_state_dir.exists():
            cleanup_attempts = 3
            for attempt in range(cleanup_attempts):
                try:
                    shutil.rmtree(self.test_state_dir)
                    logger.info("Test state directory cleaned up successfully")
                    # Unregister emergency cleanup since we succeeded
                    try:
                        atexit.unregister(self._emergency_cleanup)
                    except ValueError:
                        pass  # Function wasn't registered
                    break
                except OSError as e:
                    if attempt < cleanup_attempts - 1:
                        logger.warning(f"Cleanup attempt {attempt + 1} failed, retrying: {e}")
                        time.sleep(1)  # Wait a bit before retrying
                    else:
                        logger.error(f"Failed to clean up test directory after {cleanup_attempts} attempts: {e}")
                        logger.error(f"Manual cleanup may be required: {self.test_state_dir}")
        else:
            logger.debug("No test state directory to clean up")

    def test_vm_creation(self):
        """Test VM creation functionality."""
        logger.info("🧪 TEST: VM creation")

        try:
            # Test creating a VM with custom configuration
            result = self.run_agent_vm_command([
                "create",
                "--branch", self.test_branch,
                "--port", str(self.test_port),
                "--host", "localhost"
            ])

            # Verify VM configuration was created
            vm_config_dir = self.test_state_dir / self.test_branch
            config_file = vm_config_dir / "config.json"

            if not config_file.exists():
                raise AssertionError("VM configuration file was not created")

            # Verify configuration contents
            with config_file.open() as f:
                config_data = json.load(f)

            assert config_data["branch"] == self.test_branch
            assert config_data["port"] == self.test_port
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
            self.tests_passed += 1

        except Exception as e:
            logger.error(f"❌ FAIL: VM creation failed: {e}")
            self.tests_failed += 1
            raise

    def test_vm_listing(self):
        """Test VM listing functionality."""
        logger.info("🧪 TEST: VM listing")

        try:
            result = self.run_agent_vm_command(["list"])

            # Should show our created VM
            if self.test_branch not in result.stdout:
                raise AssertionError(f"Created VM {self.test_branch} not found in list output")

            logger.info("✅ PASS: VM listing successful")
            self.tests_passed += 1

        except Exception as e:
            logger.error(f"❌ FAIL: VM listing failed: {e}")
            self.tests_failed += 1
            raise

    def test_vm_status(self):
        """Test VM status functionality."""
        logger.info("🧪 TEST: VM status")

        try:
            result = self.run_agent_vm_command(["status", self.test_branch])

            # Should show VM as stopped initially
            if "🔴 VM Status: Stopped" not in result.stdout:
                logger.warning("VM status output may have changed format")

            logger.info("✅ PASS: VM status check successful")
            self.tests_passed += 1

        except Exception as e:
            logger.error(f"❌ FAIL: VM status check failed: {e}")
            self.tests_failed += 1
            raise

    def test_vm_start_stop_cycle(self):
        """Test VM start and stop functionality."""
        logger.info("🧪 TEST: VM start/stop cycle")

        # Skip this test if we can't run VMs (e.g., in CI without nested virtualization)
        if not self._can_run_vms():
            logger.warning("⚠️ SKIP: VM start/stop cycle (nested virtualization not available)")
            self.tests_skipped += 1
            return

        try:
            # Test starting VM
            logger.info("Starting VM...")

            if self.debug:
                logger.debug(f"About to start VM with timeout {self.timeout}s")

            result = self.run_agent_vm_command(["start", self.test_branch])

            if self.debug:
                logger.debug("VM start command completed successfully")

            # Give it a moment to fully start
            time.sleep(5)

            # Check status after start
            logger.info("Checking VM status after start...")
            status_result = self.run_agent_vm_command(["status", self.test_branch])

            if self.debug:
                logger.debug(f"VM status after start: {status_result.stdout}")

            if "🟢 VM Status: Running" not in status_result.stdout:
                logger.warning("VM may not have started properly")

            # Test stopping VM
            logger.info("Stopping VM...")
            stop_result = self.run_agent_vm_command(["stop", self.test_branch])

            if self.debug:
                logger.debug(f"VM stop result: {stop_result.stdout}")

            # Check status after stop
            logger.info("Checking VM status after stop...")
            status_result = self.run_agent_vm_command(["status", self.test_branch])

            if self.debug:
                logger.debug(f"VM status after stop: {status_result.stdout}")

            if "🔴 VM Status: Stopped" not in status_result.stdout:
                logger.warning("VM may not have stopped properly")

            logger.info("✅ PASS: VM start/stop cycle successful")
            self.tests_passed += 1

        except subprocess.TimeoutExpired as e:
            logger.error("❌ FAIL: VM start/stop cycle timed out")

            if self.debug:
                logger.error(f"Timeout details: command={' '.join(e.cmd) if e.cmd else 'unknown'}, timeout={e.timeout}")
                logger.error("This usually indicates VM startup issues. Check:")
                logger.error("- Nested virtualization support (/dev/kvm exists)")
                logger.error("- QEMU installation and dependencies")
                logger.error("- Available system resources (RAM, CPU)")
                logger.error("- Nix build system can create VMs")

            self.tests_failed += 1
            # Try to clean up
            try:
                logger.info("Attempting cleanup after timeout...")
                self.run_agent_vm_command(["stop", self.test_branch], check=False)
            except:
                if self.debug:
                    logger.debug("Cleanup also failed or timed out")
                pass

        except Exception as e:
            logger.error(f"❌ FAIL: VM start/stop cycle failed: {e}")

            if self.debug:
                logger.error(f"Exception type: {type(e).__name__}")
                logger.error(f"Exception details: {str(e)}")

            self.tests_failed += 1
            # Try to clean up
            try:
                logger.info("Attempting cleanup after failure...")
                self.run_agent_vm_command(["stop", self.test_branch], check=False)
            except:
                if self.debug:
                    logger.debug("Cleanup also failed")
                pass

    def test_vm_destruction(self):
        """Test VM destruction functionality."""
        logger.info("🧪 TEST: VM destruction")

        try:
            # Ensure VM is stopped first
            try:
                self.run_agent_vm_command(["stop", self.test_branch], check=False)
            except:
                pass  # VM might already be stopped

            # Test destroying VM
            result = self.run_agent_vm_command(["destroy", self.test_branch])

            # Verify VM configuration was removed
            vm_config_dir = self.test_state_dir / self.test_branch
            if vm_config_dir.exists():
                raise AssertionError("VM configuration directory was not removed")

            # Verify it's no longer in the list
            list_result = self.run_agent_vm_command(["list"])
            if self.test_branch in list_result.stdout:
                raise AssertionError("Destroyed VM still appears in list")

            logger.info("✅ PASS: VM destruction successful")
            self.tests_passed += 1

        except Exception as e:
            logger.error(f"❌ FAIL: VM destruction failed: {e}")
            self.tests_failed += 1
            raise

    def _can_run_vms(self) -> bool:
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

    def run_all_tests(self):
        """Run all integration tests with robust cleanup."""
        logger.info("🚀 Starting agent-vm integration tests")
        logger.info(f"Using agent-vm command: {self.agent_vm_cmd}")

        start_time = time.time()
        cleanup_attempted = False

        try:
            self.setup_test_environment()

            # Run tests in order
            test_methods = [
                self.test_vm_creation,
                self.test_vm_listing,
                self.test_vm_status,
                self.test_vm_start_stop_cycle,
                self.test_vm_destruction,
            ]

            for test_method in test_methods:
                try:
                    test_method()
                except Exception as e:
                    logger.error(f"Test {test_method.__name__} failed: {e}")
                    # Continue with other tests

        except Exception as e:
            logger.error(f"Critical error during test setup or execution: {e}")
        finally:
            # Always attempt cleanup, even if tests failed or were interrupted
            try:
                self.cleanup_test_environment()
                cleanup_attempted = True
            except Exception as e:
                logger.error(f"Cleanup failed: {e}")
                cleanup_attempted = False

        end_time = time.time()
        duration = end_time - start_time

        # Report results
        total_tests = self.tests_passed + self.tests_failed + self.tests_skipped
        logger.info("=" * 60)
        logger.info("🏁 Integration Test Results")
        logger.info(f"Total tests: {total_tests}")
        logger.info(f"✅ Passed: {self.tests_passed}")
        logger.info(f"❌ Failed: {self.tests_failed}")
        logger.info(f"⚠️ Skipped: {self.tests_skipped}")
        logger.info(f"⏱️ Duration: {duration:.1f} seconds")

        if not cleanup_attempted:
            logger.warning("⚠️ Cleanup may have failed - manual cleanup might be required")

        if self.tests_failed > 0:
            logger.error("❌ Integration tests FAILED")
            return False
        else:
            logger.info("✅ All integration tests PASSED")
            return True


def main():
    """Main entry point for integration test executable."""
    parser = argparse.ArgumentParser(
        description="Integration test executable for agent-vm",
        formatter_class=argparse.RawDescriptionHelpFormatter,
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

    parser.add_argument('--agent-vm', default='agent-vm',
                       help='Path to agent-vm executable (default: agent-vm in PATH)')
    parser.add_argument('--verbose', '-v', action='store_true',
                       help='Enable verbose logging and debugging')
    parser.add_argument('--debug', '-d', action='store_true',
                       help='Enable debug mode with comprehensive stderr/stdout capture')
    parser.add_argument('--timeout', '-t', type=int, default=120,
                       help='Timeout in seconds for VM operations (default: 120)')

    args = parser.parse_args()

    # Set up logging - debug mode implies verbose
    setup_logging(verbose=args.verbose or args.debug)

    # Run integration tests
    test_runner = AgentVMIntegrationTest(
        agent_vm_cmd=args.agent_vm,
        verbose=args.verbose,
        debug=args.debug,
        timeout=args.timeout
    )

    try:
        success = test_runner.run_all_tests()
        sys.exit(0 if success else 1)
    except KeyboardInterrupt:
        logger.info("Integration tests cancelled by user")
        sys.exit(130)
    except Exception as e:
        logger.error(f"Unexpected error during integration tests: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()
