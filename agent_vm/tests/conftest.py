"""
Test configuration and fixtures for agent-vm tests.
"""

import tempfile
import json
from pathlib import Path
from unittest.mock import Mock, patch
import pytest
import logging


@pytest.fixture
def temp_home_dir():
    """Create a temporary home directory for testing."""
    with tempfile.TemporaryDirectory() as temp_dir:
        yield Path(temp_dir)


@pytest.fixture
def mock_vm_controller(temp_home_dir):
    """Create a VMController instance with mocked home directory."""
    with patch("agent_vm.vm_controller.Path.home", return_value=temp_home_dir):
        from agent_vm.vm_controller import VMController
        return VMController()


@pytest.fixture
def mock_subprocess_run():
    """Mock subprocess.run for testing."""
    with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
        mock_run.return_value = Mock(returncode=0, stdout="", stderr="")
        yield mock_run


@pytest.fixture
def mock_os_kill():
    """Mock os.kill for testing."""
    with patch("agent_vm.vm_controller.os.kill") as mock_kill:
        yield mock_kill


@pytest.fixture
def mock_git_operations():
    """Mock git operations for testing."""
    with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
        mock_run.return_value = Mock(returncode=0, stdout="/fake/repo/path\n")
        yield mock_run


@pytest.fixture
def setup_vm_config(mock_vm_controller, temp_home_dir):
    """Set up a VM configuration for testing."""
    vm_config_dir = temp_home_dir / "agent-vms" / "test-branch"
    vm_config_dir.mkdir(parents=True)

    # Create config.json
    config_data = {
        "branch": "test-branch",
        "vm_name": "agent-dev-test-branch",
        "ssh_key_path": str(vm_config_dir / "ssh" / "id_ed25519"),
        "workspace_path": str(vm_config_dir / "workspace"),
        "port": 8000
    }

    config_file = vm_config_dir / "config.json"
    with config_file.open('w') as f:
        json.dump(config_data, f)

    # Create SSH directory
    ssh_dir = vm_config_dir / "ssh"
    ssh_dir.mkdir()

    # Create mock SSH keys
    (ssh_dir / "id_ed25519").write_text("mock-private-key")
    (ssh_dir / "id_ed25519.pub").write_text("mock-public-key")

    # Create workspace directory
    workspace_dir = vm_config_dir / "workspace"
    workspace_dir.mkdir()

    mock_vm_controller.base_dir = temp_home_dir / "agent-vms"
    return config_data


@pytest.fixture
def capture_logs(caplog):
    """Capture logs for testing."""
    caplog.set_level(logging.INFO, logger="agent_vm.vm_controller")
    return caplog


# Test markers
pytest_plugins = ["pytest_mock"]
