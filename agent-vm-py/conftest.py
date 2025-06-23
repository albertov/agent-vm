"""
Pytest configuration and fixtures for integration tests.

This conftest.py provides function-scoped fixtures to ensure test isolation
by giving each test its own unique temporary state directory.
"""

import logging
import shutil
import tempfile
import time
from pathlib import Path

import pytest

logger = logging.getLogger(__name__)


def pytest_configure(config):
    """Configure pytest with custom markers for integration tests."""
    markers = [
        "integration: integration tests that test end-to-end functionality",
        "vm: tests that require VM operations",
        "basic: basic functionality tests",
        "slow: tests that may take longer to complete",
        "ssh: tests that involve SSH connectivity",
        "git: tests that involve git operations"
    ]

    for marker in markers:
        config.addinivalue_line("markers", marker)


@pytest.fixture(scope="function")
def isolated_state_dir():
    """
    Function-scoped fixture providing a unique temporary state directory for each test.

    This ensures complete isolation between tests by giving each test its own
    temporary directory for agent-vm state, preventing interference between tests.

    Returns:
        Path: Absolute path to a unique temporary directory for this test.
    """
    # Create unique temp directory with timestamp to avoid conflicts
    timestamp = int(time.time() * 1000000)  # microsecond precision
    prefix = f"agent-vm-test-{timestamp}-"

    test_state_dir = Path(tempfile.mkdtemp(prefix=prefix))
    logger.debug(f"Created isolated state directory for test: {test_state_dir}")

    yield test_state_dir

    # Cleanup after test
    if test_state_dir.exists():
        cleanup_attempts = 3
        for attempt in range(cleanup_attempts):
            try:
                shutil.rmtree(test_state_dir)
                logger.debug(f"Cleaned up test state directory: {test_state_dir}")
                break
            except OSError as e:
                if attempt < cleanup_attempts - 1:
                    logger.warning(f"Cleanup attempt {attempt + 1} failed, retrying: {e}")
                    time.sleep(0.5)  # Brief pause before retry
                else:
                    logger.error(f"Failed to clean up test directory after {cleanup_attempts} attempts: {e}")
                    logger.error(f"Manual cleanup may be required: {test_state_dir}")


@pytest.fixture(scope="function")
def isolated_vm_config(isolated_state_dir):
    """
    Function-scoped fixture providing isolated VM configuration for each test.

    This builds on the isolated_state_dir fixture to provide a complete
    test configuration including a unique branch name and port.

    Args:
        isolated_state_dir: The isolated state directory from the fixture above.

    Returns:
        Dict: Test configuration with state_dir, branch, and port.
    """
    # Generate unique branch name with timestamp
    timestamp = int(time.time() * 1000000)
    test_branch = f"test-{timestamp}"

    # Use a predictable port range for testing (avoid conflicts)
    base_port = 13000 + (timestamp % 1000)  # Port range 13000-13999

    config = {
        "state_dir": isolated_state_dir,
        "branch": test_branch,
        "port": base_port
    }

    logger.debug(f"Created isolated VM config: {config}")
    return config
