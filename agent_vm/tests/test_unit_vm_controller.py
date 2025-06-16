"""
Unit tests for VMController class.

These tests focus on individual methods and their behavior in isolation.
"""

import json
import os
import tempfile
import time
from pathlib import Path
from unittest.mock import Mock, patch, call, MagicMock
import pytest
from agent_vm.vm_controller import VMController


@pytest.mark.unit
class TestVMControllerInit:
    """Test VMController initialization."""

    def test_init_sets_paths(self, temp_home_dir):
        """Test that VMController sets up correct paths."""
        with patch("agent_vm.vm_controller.Path.home", return_value=temp_home_dir):
            controller = VMController()

            assert controller.home_dir == temp_home_dir
            assert controller.base_dir == temp_home_dir / ".local" / "share" / "agent-vms"

    def test_init_gets_origin_repo(self, mock_git_operations):
        """Test that VMController gets origin repo on init."""
        controller = VMController()

        # Should call git remote get-url origin
        mock_git_operations.assert_called()
        assert controller.origin_repo == "/fake/repo/path"


@pytest.mark.unit
class TestGetOriginRepo:
    """Test _get_origin_repo method."""

    def test_get_origin_repo_success(self, mock_vm_controller, mock_subprocess_run):
        """Test getting origin repo successfully."""
        mock_subprocess_run.return_value = Mock(
            returncode=0,
            stdout="https://github.com/user/repo.git\n"
        )

        result = mock_vm_controller._get_origin_repo()

        assert result == "https://github.com/user/repo.git"
        mock_subprocess_run.assert_called_with(
            ["git", "remote", "get-url", "origin"],
            capture_output=True,
            text=True,
            check=True
        )

    def test_get_origin_repo_fallback_to_toplevel(self, mock_vm_controller):
        """Test fallback to git toplevel when origin fails."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            # First call (get-url) fails, second call (rev-parse) succeeds
            mock_run.side_effect = [
                Mock(side_effect=Exception("Command failed")),
                Mock(returncode=0, stdout="/path/to/repo\n")
            ]

            result = mock_vm_controller._get_origin_repo()

            assert result == "/path/to/repo"
            assert mock_run.call_count == 2


@pytest.mark.unit
class TestGetCurrentBranch:
    """Test _get_current_branch method."""

    def test_get_current_branch_success(self, mock_vm_controller, mock_subprocess_run):
        """Test getting current branch successfully."""
        mock_subprocess_run.return_value = Mock(
            returncode=0,
            stdout="feature-branch\n"
        )

        result = mock_vm_controller._get_current_branch()

        assert result == "feature-branch"

    def test_get_current_branch_detached_head(self, mock_vm_controller):
        """Test handling detached HEAD state."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            # First call returns empty (detached HEAD), second returns commit hash
            mock_run.side_effect = [
                Mock(returncode=0, stdout="\n"),
                Mock(returncode=0, stdout="abc123\n")
            ]

            result = mock_vm_controller._get_current_branch()

            assert result == "detached-abc123"

    def test_get_current_branch_no_git(self, mock_vm_controller, mock_subprocess_run):
        """Test handling when git is not available."""
        mock_subprocess_run.side_effect = Exception("Git not found")

        result = mock_vm_controller._get_current_branch()

        assert result == "default"


@pytest.mark.unit
class TestCleanupStaleProcesses:
    """Test _cleanup_stale_processes method."""

    def test_cleanup_stale_processes_found(self, mock_vm_controller, mock_os_kill):
        """Test cleaning up stale processes when found."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stdout="1234\n5678\n")

            mock_vm_controller._cleanup_stale_processes("test-vm")

            # Should kill both processes
            assert mock_os_kill.call_count == 4  # SIGTERM and SIGKILL for each
            mock_os_kill.assert_any_call(1234, 15)  # SIGTERM
            mock_os_kill.assert_any_call(1234, 9)   # SIGKILL
            mock_os_kill.assert_any_call(5678, 15)  # SIGTERM
            mock_os_kill.assert_any_call(5678, 9)   # SIGKILL

    def test_cleanup_stale_processes_none_found(self, mock_vm_controller, mock_os_kill):
        """Test when no stale processes are found."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.side_effect = Exception("No processes found")

            mock_vm_controller._cleanup_stale_processes("test-vm")

            # Should not attempt to kill any processes
            mock_os_kill.assert_not_called()


@pytest.mark.unit
class TestIsVMRunning:
    """Test _is_vm_running method."""

    def test_vm_is_running(self, mock_vm_controller, mock_subprocess_run):
        """Test detecting running VM."""
        mock_subprocess_run.return_value = Mock(returncode=0)

        result = mock_vm_controller._is_vm_running("test-vm")

        assert result is True
        mock_subprocess_run.assert_called_with(
            ["pgrep", "-f", "qemu.*test-vm"],
            capture_output=True,
            check=True
        )

    def test_vm_not_running(self, mock_vm_controller, mock_subprocess_run):
        """Test when VM is not running."""
        mock_subprocess_run.side_effect = Exception("Process not found")

        result = mock_vm_controller._is_vm_running("test-vm")

        assert result is False


@pytest.mark.unit
class TestWaitForVMReady:
    """Test _wait_for_vm_ready method."""

    def test_wait_for_vm_ready_success(self, mock_vm_controller):
        """Test successful VM ready detection."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0)

            result = mock_vm_controller._wait_for_vm_ready(ssh_key, max_attempts=2)

            assert result is True

    def test_wait_for_vm_ready_timeout(self, mock_vm_controller):
        """Test VM ready timeout."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.time.sleep"), \
             patch("builtins.print"):
            mock_run.side_effect = Exception("Connection failed")

            result = mock_vm_controller._wait_for_vm_ready(ssh_key, max_attempts=2)

            assert result is False


@pytest.mark.unit
class TestVMStatusMethods:
    """Test VM status checking methods."""

    def test_check_ssh_connectivity_success(self, mock_vm_controller):
        """Test successful SSH connectivity check."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=0, stdout="SSH OK")

            result = mock_vm_controller._check_ssh_connectivity(ssh_key)

            assert result['connected'] is True
            assert 'error' not in result

    def test_check_ssh_connectivity_failure(self, mock_vm_controller):
        """Test failed SSH connectivity check."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(returncode=1, stderr="Connection refused")

            result = mock_vm_controller._check_ssh_connectivity(ssh_key)

            assert result['connected'] is False
            assert result['error'] == "Connection refused"

    def test_check_agent_service_detailed_active(self, mock_vm_controller):
        """Test active agent service detection."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=0,
                stdout="ActiveState=active\nSubState=running\nMainPID=1234\nNRestarts=0\nMemoryCurrent=52428800"
            )

            result = mock_vm_controller._check_agent_service_detailed(ssh_key)

            assert result['active'] is True
            assert result['substate'] == 'running'
            assert result['pid'] == '1234'
            assert result['restart_count'] == '0'
            assert result['memory'] == '50 MB'

    def test_check_mcp_proxy_health_healthy(self, mock_vm_controller):
        """Test healthy MCP proxy detection."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.time.time", side_effect=[1000.0, 1000.1]):
            mock_run.return_value = Mock(returncode=0, stdout="OK")

            result = mock_vm_controller._check_mcp_proxy_health(ssh_key, 8000)

            assert result['healthy'] is True
            assert result['response_time'] == 100  # 0.1 seconds = 100ms

    def test_check_workspace_status_accessible(self, mock_vm_controller):
        """Test accessible workspace detection."""
        ssh_key = Path("/fake/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=0,
                stdout="/workspace\n1.2G\t.\n0"
            )

            result = mock_vm_controller._check_workspace_status(ssh_key, "/workspace")

            assert result['accessible'] is True
            assert result['size'] == "1.2G\t."
            assert result['git_status'] == "clean"


@pytest.mark.unit
class TestCreateVM:
    """Test create_vm method."""

    def test_create_vm_success(self, mock_vm_controller, mock_git_operations, temp_home_dir):
        """Test successful VM creation."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            # Mock SSH key generation
            mock_run.return_value = Mock(returncode=0)

            mock_vm_controller.create_vm("localhost", 8000, "test-branch")

            # Verify VM directory structure was created
            vm_dir = mock_vm_controller.base_dir / "test-branch"
            assert vm_dir.exists()
            assert (vm_dir / "config.json").exists()
            assert (vm_dir / "ssh").exists()

    def test_create_vm_already_exists(self, mock_vm_controller, temp_home_dir):
        """Test creating VM when configuration already exists."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"
        vm_dir = mock_vm_controller.base_dir / "test-branch"
        vm_dir.mkdir(parents=True)

        with pytest.raises(SystemExit):
            mock_vm_controller.create_vm("localhost", 8000, "test-branch")


@pytest.mark.unit
class TestStopVM:
    """Test stop_vm method."""

    def test_stop_vm_by_pid_success(self, mock_vm_controller, temp_home_dir, mock_os_kill):
        """Test stopping VM using PID file."""
        vm_config_dir = temp_home_dir / "test-vm"
        vm_config_dir.mkdir(parents=True)

        # Create PID file
        pid_file = vm_config_dir / "vm.pid"
        pid_file.write_text("1234")

        # Create config file
        config = {"vm_name": "test-vm"}
        config_file = vm_config_dir / "config.json"
        with config_file.open('w') as f:
            json.dump(config, f)

        mock_vm_controller._stop_vm_by_pid(vm_config_dir)

        # Should kill process with SIGTERM
        mock_os_kill.assert_any_call(1234, 15)

        # PID file should be removed
        assert not pid_file.exists()


@pytest.mark.unit
class TestListVMs:
    """Test list_vms method."""

    def test_list_vms_empty(self, mock_vm_controller, temp_home_dir, capture_logs):
        """Test listing VMs when none exist."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        mock_vm_controller.list_vms()

        assert "No VM configurations found" in capture_logs.text

    def test_list_vms_with_configs(self, mock_vm_controller, temp_home_dir, capture_logs):
        """Test listing VMs with existing configurations."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        # Create test VM configs
        for branch in ["main", "feature-x"]:
            vm_dir = mock_vm_controller.base_dir / branch
            vm_dir.mkdir(parents=True)

            config = {
                "branch": branch,
                "created_at": "2025-06-16T14:30:00+02:00"
            }
            config_file = vm_dir / "config.json"
            with config_file.open('w') as f:
                json.dump(config, f)

        mock_vm_controller.list_vms()

        assert "Available VM configurations:" in capture_logs.text
        assert "main" in capture_logs.text
        assert "feature-x" in capture_logs.text


@pytest.mark.unit
class TestDestroyVM:
    """Test destroy_vm method."""

    def test_destroy_vm_success(self, mock_vm_controller, temp_home_dir):
        """Test successful VM destruction."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        # Create VM directory
        vm_dir = mock_vm_controller.base_dir / "test-branch"
        vm_dir.mkdir(parents=True)

        config = {"vm_name": "agent-dev-test-branch"}
        config_file = vm_dir / "config.json"
        with config_file.open('w') as f:
            json.dump(config, f)

        with patch.object(mock_vm_controller, '_is_vm_running', return_value=False):
            mock_vm_controller.destroy_vm("test-branch")

        # VM directory should be removed
        assert not vm_dir.exists()

    def test_destroy_vm_not_found(self, mock_vm_controller, temp_home_dir):
        """Test destroying non-existent VM."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with pytest.raises(SystemExit):
            mock_vm_controller.destroy_vm("nonexistent-branch")


@pytest.mark.unit
class TestVMShell:
    """Test vm_shell method."""

    def test_vm_shell_success(self, mock_vm_controller, setup_vm_config, mock_subprocess_run):
        """Test opening VM shell successfully."""
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=True):
            mock_vm_controller.vm_shell("test-branch")

        # Should call SSH command
        mock_subprocess_run.assert_called()
        call_args = mock_subprocess_run.call_args[0][0]
        assert "ssh" in call_args
        assert "-p" in call_args
        assert "2222" in call_args

    def test_vm_shell_not_running(self, mock_vm_controller, setup_vm_config):
        """Test opening shell when VM is not running."""
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=False), \
             pytest.raises(SystemExit):
            mock_vm_controller.vm_shell("test-branch")


@pytest.mark.unit
class TestVMLogs:
    """Test vm_logs method."""

    def test_vm_logs_running(self, mock_vm_controller, setup_vm_config, mock_subprocess_run):
        """Test viewing logs when VM is running."""
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=True):
            mock_vm_controller.vm_logs("test-branch")

        # Should call SSH with journalctl command
        mock_subprocess_run.assert_called()
        call_args = mock_subprocess_run.call_args[0][0]
        assert "ssh" in call_args
        assert "journalctl" in call_args

    def test_vm_logs_not_running(self, mock_vm_controller, setup_vm_config, capture_logs):
        """Test viewing logs when VM is not running."""
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=False):
            mock_vm_controller.vm_logs("test-branch")

        assert "VM is not running" in capture_logs.text
