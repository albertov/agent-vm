"""
Test configuration and shared fixtures for agent-vm tests.
"""

import os
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Generator, Optional
import pytest
from unittest.mock import Mock, patch, MagicMock
import json

# Import the module under test
from vm_controller import VMController


@pytest.fixture
def temp_home_dir() -> Generator[Path, None, None]:
    """Create a temporary home directory for testing."""
    with tempfile.TemporaryDirectory() as temp_dir:
        yield Path(temp_dir)


@pytest.fixture
def mock_vm_controller(temp_home_dir: Path) -> VMController:
    """Create a VMController instance with mocked home directory."""
    controller = VMController()
    controller.home_dir = temp_home_dir
    controller.base_dir = temp_home_dir / ".local" / "share" / "agent-vms"
    controller.origin_repo = "/fake/repo/path"
    return controller


@pytest.fixture
def sample_vm_config() -> Dict:
    """Sample VM configuration data for testing."""
    return {
        "branch": "test-branch",
        "host": "localhost",
        "port": 8000,
        "config_path": "vm-config.nix",
        "workspace_path": "/tmp/test-workspace",
        "ssh_key_path": "/tmp/ssh/id_ed25519",
        "created_at": "2025-06-16T14:30:00+02:00",
        "origin_repo": "/fake/repo/path",
        "upstream_repo": "",
        "vm_name": "agent-dev-test-branch"
    }


@pytest.fixture
def setup_vm_config(mock_vm_controller: VMController, sample_vm_config: Dict) -> Path:
    """Set up a VM configuration directory with config file."""
    vm_config_dir = mock_vm_controller.base_dir / sample_vm_config["branch"]
    vm_config_dir.mkdir(parents=True, exist_ok=True)

    config_file = vm_config_dir / "config.json"
    with config_file.open('w') as f:
        json.dump(sample_vm_config, f, indent=2)

    # Create SSH directory and keys
    ssh_dir = vm_config_dir / "ssh"
    ssh_dir.mkdir(mode=0o700, exist_ok=True)

    private_key = ssh_dir / "id_ed25519"
    public_key = ssh_dir / "id_ed25519.pub"

    private_key.write_text("fake-private-key")
    private_key.chmod(0o600)
    public_key.write_text("fake-public-key")
    public_key.chmod(0o644)

    return vm_config_dir


@pytest.fixture
def mock_subprocess_run():
    """Mock subprocess.run for controlled testing."""
    with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
        # Default successful result
        mock_run.return_value = Mock(
            returncode=0,
            stdout="mocked output",
            stderr=""
        )
        yield mock_run


@pytest.fixture
def mock_subprocess_popen():
    """Mock subprocess.Popen for VM process testing."""
    with patch("agent_vm.vm_controller.subprocess.Popen") as mock_popen:
        mock_process = Mock()
        mock_process.pid = 12345
        mock_popen.return_value = mock_process
        yield mock_popen


@pytest.fixture
def mock_os_kill():
    """Mock os.kill for process management testing."""
    with patch("agent_vm.vm_controller.os.kill") as mock_kill:
        yield mock_kill


@pytest.fixture
def mock_pathlib_operations():
    """Mock pathlib operations for file system testing."""
    with patch("agent_vm.vm_controller.Path.mkdir") as mock_mkdir, \
         patch("agent_vm.vm_controller.Path.chmod") as mock_chmod, \
         patch("agent_vm.vm_controller.Path.unlink") as mock_unlink:
        yield {
            "mkdir": mock_mkdir,
            "chmod": mock_chmod,
            "unlink": mock_unlink
        }


@pytest.fixture
def capture_logs(caplog):
    """Capture log output for testing."""
    with caplog.at_level("INFO"):
        yield caplog


class MockSSHResult:
    """Mock SSH command result for testing connectivity."""

    def __init__(self, returncode: int = 0, stdout: str = "", stderr: str = ""):
        self.returncode = returncode
        self.stdout = stdout
        self.stderr = stderr


@pytest.fixture
def mock_ssh_connectivity():
    """Mock SSH connectivity for testing."""
    def _mock_ssh(connected: bool = True, error: str = ""):
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            if connected:
                mock_run.return_value = MockSSHResult(0, "SSH OK")
            else:
                mock_run.return_value = MockSSHResult(1, "", error)
            yield mock_run

    return _mock_ssh


@pytest.fixture
def mock_git_operations():
    """Mock git operations for testing."""
    with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
        def side_effect(cmd, **kwargs):
            if "git" in cmd[0]:
                if "branch" in cmd:
                    return Mock(returncode=0, stdout="test-branch\n")
                elif "remote" in cmd and "get-url" in cmd:
                    return Mock(returncode=0, stdout="/fake/repo/path\n")
                elif "rev-parse" in cmd and "--show-toplevel" in cmd:
                    return Mock(returncode=0, stdout="/fake/repo/root\n")
                elif "clone" in cmd:
                    return Mock(returncode=0)
                else:
                    return Mock(returncode=0, stdout="")
            return Mock(returncode=0, stdout="")

        mock_run.side_effect = side_effect
        yield mock_run


# Test markers for organizing tests
pytest_markers = [
    pytest.mark.unit,
    pytest.mark.integration,
    pytest.mark.slow,
    pytest.mark.vm,
    pytest.mark.ssh,
    pytest.mark.git,
]


def pytest_configure(config):
    """Configure pytest with custom markers."""
    for marker in pytest_markers:
        config.addinivalue_line("markers", f"{marker.name}: {marker.name} tests")


# Test utilities
def create_test_vm_structure(base_dir: Path, branch: str = "test") -> Path:
    """Create a complete test VM directory structure."""
    vm_dir = base_dir / branch
    vm_dir.mkdir(parents=True, exist_ok=True)

    # Create workspace
    workspace = vm_dir / "workspace"
    workspace.mkdir(exist_ok=True)

    # Create SSH keys
    ssh_dir = vm_dir / "ssh"
    ssh_dir.mkdir(mode=0o700, exist_ok=True)

    private_key = ssh_dir / "id_ed25519"
    public_key = ssh_dir / "id_ed25519.pub"

    private_key.write_text("-----BEGIN OPENSSH PRIVATE KEY-----\nfake-key-data\n-----END OPENSSH PRIVATE KEY-----")
    private_key.chmod(0o600)

    public_key.write_text("ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFakeKeyData vm-key-test")
    public_key.chmod(0o644)

    # Create config
    config = {
        "branch": branch,
        "host": "localhost",
        "port": 8000,
        "config_path": "vm-config.nix",
        "workspace_path": str(workspace),
        "ssh_key_path": str(private_key),
        "created_at": "2025-06-16T14:30:00+02:00",
        "origin_repo": "/fake/repo",
        "upstream_repo": "",
        "vm_name": f"agent-dev-{branch}"
    }

    config_file = vm_dir / "config.json"
    with config_file.open('w') as f:
        json.dump(config, f, indent=2)

    return vm_dir


def assert_vm_directory_structure(vm_dir: Path):
    """Assert that a VM directory has the correct structure."""
    assert vm_dir.exists()
    assert (vm_dir / "config.json").exists()
    assert (vm_dir / "ssh").exists()
    assert (vm_dir / "ssh" / "id_ed25519").exists()
    assert (vm_dir / "ssh" / "id_ed25519.pub").exists()
    assert (vm_dir / "workspace").exists()


def assert_config_file_valid(config_file: Path):
    """Assert that a config file is valid JSON with required fields."""
    assert config_file.exists()

    with config_file.open() as f:
        config = json.load(f)

    required_fields = [
        "branch", "host", "port", "config_path", "workspace_path",
        "ssh_key_path", "created_at", "origin_repo", "vm_name"
    ]

    for field in required_fields:
        assert field in config, f"Missing required field: {field}"
