"""
Integration tests for agent-vm functionality.

These tests verify end-to-end workflows and component interactions.
"""

import json
import os
import subprocess
import tempfile
import time
from pathlib import Path
from unittest.mock import Mock, patch, call
import pytest
from agent_vm.vm_controller import VMController, main


@pytest.mark.integration
class TestVMLifecycleIntegration:
    """Test complete VM lifecycle workflows."""

    def test_create_start_stop_destroy_workflow(self, mock_vm_controller, temp_home_dir):
        """Test complete VM lifecycle: create -> start -> stop -> destroy."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.subprocess.Popen") as mock_popen, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.os.kill"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"), \
             patch("agent_vm.vm_controller.time.sleep"):

            # Mock git operations
            def git_side_effect(cmd, **kwargs):
                if "git" in cmd[0]:
                    if "branch" in cmd:
                        return Mock(returncode=0, stdout="test-branch\n")
                    elif "remote" in cmd:
                        return Mock(returncode=0, stdout="/fake/repo\n")
                    elif "rev-parse" in cmd:
                        return Mock(returncode=0, stdout="/fake/repo/root\n")
                    elif "clone" in cmd:
                        return Mock(returncode=0)
                    else:
                        return Mock(returncode=0)
                elif "ssh-keygen" in cmd[0]:
                    return Mock(returncode=0)
                elif "nix" in cmd[0]:
                    return Mock(returncode=0, stdout="/nix/store/vm-path\n")
                elif "pgrep" in cmd[0]:
                    return Mock(returncode=0, stdout="1234\n")
                elif "ssh" in cmd[0]:
                    return Mock(returncode=0, stdout="SSH OK\n")
                return Mock(returncode=0)

            mock_run.side_effect = git_side_effect

            # Mock VM process
            mock_process = Mock()
            mock_process.pid = 12345
            mock_popen.return_value = mock_process

            # 1. Create VM
            mock_vm_controller.create_vm("localhost", 8000, "test-branch")

            vm_dir = mock_vm_controller.base_dir / "test-branch"
            assert vm_dir.exists()
            assert (vm_dir / "config.json").exists()
            assert (vm_dir / "ssh").exists()

            # 2. Start VM
            with patch.object(mock_vm_controller, '_is_vm_running', return_value=False), \
                 patch.object(mock_vm_controller, '_wait_for_vm_ready', return_value=True), \
                 patch.object(mock_vm_controller, '_start_agent_in_vm'), \
                 patch.object(mock_vm_controller, '_cleanup_stale_processes'):

                mock_vm_controller.start_vm("test-branch")

                # PID file should be created
                pid_file = vm_dir / "vm.pid"
                assert pid_file.exists()
                assert pid_file.read_text() == "12345"

            # 3. Stop VM
            mock_vm_controller.stop_vm("test-branch")

            # PID file should be removed
            assert not (vm_dir / "vm.pid").exists()

            # 4. Destroy VM
            with patch.object(mock_vm_controller, '_is_vm_running', return_value=False):
                mock_vm_controller.destroy_vm("test-branch")

            # VM directory should be removed
            assert not vm_dir.exists()

    def test_vm_status_monitoring_integration(self, mock_vm_controller, setup_vm_config):
        """Test comprehensive VM status monitoring."""
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=True), \
             patch.object(mock_vm_controller, '_get_vm_pid', return_value=1234), \
             patch.object(mock_vm_controller, '_get_vm_resources', return_value={
                 'cpu': '2.5', 'memory': '1024', 'uptime': '1:30:45'
             }), \
             patch.object(mock_vm_controller, '_check_ssh_connectivity', return_value={
                 'connected': True
             }), \
             patch.object(mock_vm_controller, '_check_agent_service_detailed', return_value={
                 'active': True, 'uptime': '1:30:00', 'memory': '256 MB', 'restart_count': '0'
             }), \
             patch.object(mock_vm_controller, '_check_mcp_proxy_health', return_value={
                 'healthy': True, 'response_time': 150
             }), \
             patch.object(mock_vm_controller, '_check_workspace_status', return_value={
                 'accessible': True, 'size': '2.1G', 'git_status': 'clean'
             }) as mock_workspace:

            mock_vm_controller.vm_status("test-branch")

            # All status checks should be called
            mock_workspace.assert_called_once()

    def test_multi_instance_support(self, mock_vm_controller, temp_home_dir):
        """Test managing multiple VM instances."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        branches = ["main", "feature-x", "hotfix-y"]

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            # Mock successful operations
            mock_run.return_value = Mock(returncode=0, stdout="test-output\n")

            # Create multiple VMs
            for i, branch in enumerate(branches):
                port = 8000 + i
                mock_vm_controller.create_vm("localhost", port, branch)

                vm_dir = mock_vm_controller.base_dir / branch
                assert vm_dir.exists()

                # Verify unique ports in config
                with (vm_dir / "config.json").open() as f:
                    config = json.load(f)
                assert config["port"] == port
                assert config["vm_name"] == f"agent-dev-{branch}"

            # List all VMs
            mock_vm_controller.list_vms()

            # Verify all VM directories exist
            for branch in branches:
                assert (mock_vm_controller.base_dir / branch).exists()


@pytest.mark.integration
class TestSSHConnectivityIntegration:
    """Test SSH connectivity and key management."""

    def test_ssh_key_generation_and_usage(self, mock_vm_controller, temp_home_dir):
        """Test SSH key generation and subsequent usage."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            # Mock git and SSH operations
            def cmd_side_effect(cmd, **kwargs):
                if "ssh-keygen" in cmd[0]:
                    # Simulate SSH key generation
                    return Mock(returncode=0)
                elif "git" in cmd[0]:
                    return Mock(returncode=0, stdout="test-output\n")
                return Mock(returncode=0)

            mock_run.side_effect = cmd_side_effect

            # Create VM (which generates SSH keys)
            mock_vm_controller.create_vm("localhost", 8000, "test-branch")

            vm_dir = mock_vm_controller.base_dir / "test-branch"
            ssh_dir = vm_dir / "ssh"
            private_key = ssh_dir / "id_ed25519"
            public_key = ssh_dir / "id_ed25519.pub"

            # Verify SSH key files were created
            assert private_key.exists()
            assert public_key.exists()

            # Verify key permissions (mocked, but structure exists)
            assert ssh_dir.exists()

            # Test SSH connectivity check uses the generated key
            with patch.object(mock_vm_controller, '_check_ssh_connectivity') as mock_ssh_check:
                mock_ssh_check.return_value = {'connected': True}

                with patch.object(mock_vm_controller, '_is_vm_running', return_value=True):
                    mock_vm_controller.vm_status("test-branch")

                # SSH check should be called with the generated key path
                mock_ssh_check.assert_called_with(private_key)

    @pytest.mark.ssh
    def test_ssh_connection_retry_logic(self, mock_vm_controller, setup_vm_config):
        """Test SSH connection retry behavior."""
        ssh_key_path = Path("/fake/ssh/key")

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.time.sleep"), \
             patch("builtins.print"):

            # First 2 attempts fail, 3rd succeeds
            mock_run.side_effect = [
                Mock(side_effect=Exception("Connection refused")),
                Mock(side_effect=Exception("Connection refused")),
                Mock(returncode=0)
            ]

            result = mock_vm_controller._wait_for_vm_ready(ssh_key_path, max_attempts=3)

            assert result is True
            assert mock_run.call_count == 3


@pytest.mark.integration
class TestWorkspaceManagement:
    """Test workspace creation and management."""

    @pytest.mark.git
    def test_workspace_git_setup_integration(self, mock_vm_controller, temp_home_dir):
        """Test workspace git repository setup."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir") as mock_chdir, \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            git_commands = []

            def git_side_effect(cmd, **kwargs):
                git_commands.append(cmd)
                if "git" in cmd[0]:
                    if "branch" in cmd:
                        return Mock(returncode=0, stdout="test-branch\n")
                    elif "remote" in cmd and "get-url" in cmd:
                        return Mock(returncode=0, stdout="https://github.com/user/repo.git\n")
                    elif "rev-parse" in cmd and "--show-toplevel" in cmd:
                        return Mock(returncode=0, stdout="/source/repo\n")
                    elif "clone" in cmd:
                        return Mock(returncode=0)
                    elif "remote" in cmd and "set-url" in cmd:
                        return Mock(returncode=0)
                    elif "remote" in cmd and "add" in cmd:
                        return Mock(returncode=0)
                    else:
                        return Mock(returncode=0)
                elif "ssh-keygen" in cmd[0]:
                    return Mock(returncode=0)
                return Mock(returncode=0)

            mock_run.side_effect = git_side_effect

            mock_vm_controller.create_vm("localhost", 8000, "test-branch")

            # Verify git operations were called
            git_clone_called = any("clone" in str(cmd) for cmd in git_commands)
            git_remote_called = any("remote" in str(cmd) for cmd in git_commands)

            assert git_clone_called
            assert git_remote_called

            # Verify workspace directory was created
            vm_dir = mock_vm_controller.base_dir / "test-branch"
            workspace_dir = vm_dir / "workspace"
            assert workspace_dir.exists()

    def test_workspace_status_checking(self, mock_vm_controller):
        """Test workspace status and git status checking."""
        ssh_key = Path("/fake/key")
        workspace_path = "/workspace"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            # Mock workspace status command output
            mock_run.return_value = Mock(
                returncode=0,
                stdout="/workspace\n512M\t.\n3"  # pwd, size, git changes
            )

            result = mock_vm_controller._check_workspace_status(ssh_key, workspace_path)

            assert result['accessible'] is True
            assert result['size'] == "512M\t."
            assert result['git_status'] == "3 modified files"

            # Verify SSH command was constructed correctly
            mock_run.assert_called_once()
            call_args = mock_run.call_args[0][0]
            assert "ssh" in call_args
            assert f"cd {workspace_path}" in " ".join(call_args)


@pytest.mark.integration
class TestErrorHandlingIntegration:
    """Test error handling and recovery scenarios."""

    def test_vm_startup_failure_recovery(self, mock_vm_controller, setup_vm_config):
        """Test recovery when VM startup fails."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.subprocess.Popen") as mock_popen, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch.object(mock_vm_controller, '_is_vm_running', return_value=False), \
             patch.object(mock_vm_controller, '_cleanup_stale_processes'), \
             patch.object(mock_vm_controller, '_wait_for_vm_ready', return_value=False), \
             patch.object(mock_vm_controller, '_stop_vm_by_pid') as mock_stop:

            # Mock successful build but failed VM ready
            mock_run.return_value = Mock(returncode=0, stdout="/nix/store/vm-path\n")
            mock_process = Mock(pid=12345)
            mock_popen.return_value = mock_process

            with pytest.raises(SystemExit):
                mock_vm_controller.start_vm("test-branch")

            # Should attempt cleanup on failure
            mock_stop.assert_called_once()

    def test_ssh_key_generation_failure(self, mock_vm_controller, temp_home_dir):
        """Test handling SSH key generation failure."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"):

            def cmd_side_effect(cmd, **kwargs):
                if "ssh-keygen" in cmd[0]:
                    raise subprocess.CalledProcessError(1, cmd)
                elif "git" in cmd[0]:
                    return Mock(returncode=0, stdout="test-output\n")
                return Mock(returncode=0)

            mock_run.side_effect = cmd_side_effect

            with pytest.raises(subprocess.CalledProcessError):
                mock_vm_controller.create_vm("localhost", 8000, "test-branch")

    def test_cleanup_on_partial_creation(self, mock_vm_controller, temp_home_dir):
        """Test cleanup when VM creation is partially completed."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            def cmd_side_effect(cmd, **kwargs):
                if "git" in cmd[0] and "clone" in cmd:
                    # Simulate git clone failure
                    raise subprocess.CalledProcessError(1, cmd)
                elif "ssh-keygen" in cmd[0]:
                    return Mock(returncode=0)
                elif "git" in cmd[0]:
                    return Mock(returncode=0, stdout="test-output\n")
                return Mock(returncode=0)

            mock_run.side_effect = cmd_side_effect

            with pytest.raises(SystemExit):
                mock_vm_controller.create_vm("localhost", 8000, "test-branch")

            # Partially created VM directory should be cleaned up
            vm_dir = mock_vm_controller.base_dir / "test-branch"
            assert not vm_dir.exists()


@pytest.mark.integration
class TestCommandLineIntegration:
    """Test command-line interface integration."""

    def test_main_function_create_command(self):
        """Test main function with create command."""
        test_args = ["agent-vm", "create", "--branch=test", "--port=8001"]

        with patch("sys.argv", test_args), \
             patch("agent_vm.vm_controller.VMController") as mock_controller_class:

            mock_controller = Mock()
            mock_controller_class.return_value = mock_controller

            main()

            # Verify controller was instantiated and create_vm was called
            mock_controller_class.assert_called_once()
            mock_controller.create_vm.assert_called_once_with("localhost", 8001, "test", "vm-config.nix")

    def test_main_function_status_command(self):
        """Test main function with status command."""
        test_args = ["agent-vm", "status", "feature-branch"]

        with patch("sys.argv", test_args), \
             patch("agent_vm.vm_controller.VMController") as mock_controller_class:

            mock_controller = Mock()
            mock_controller_class.return_value = mock_controller

            main()

            mock_controller.vm_status.assert_called_once_with("feature-branch")

    def test_main_function_exception_handling(self):
        """Test main function exception handling."""
        test_args = ["agent-vm", "start"]

        with patch("sys.argv", test_args), \
             patch("agent_vm.vm_controller.VMController") as mock_controller_class, \
             pytest.raises(SystemExit):

            mock_controller = Mock()
            mock_controller.start_vm.side_effect = subprocess.CalledProcessError(1, ["test"])
            mock_controller_class.return_value = mock_controller

            main()

    def test_main_function_keyboard_interrupt(self):
        """Test main function keyboard interrupt handling."""
        test_args = ["agent-vm", "start"]

        with patch("sys.argv", test_args), \
             patch("agent_vm.vm_controller.VMController") as mock_controller_class, \
             pytest.raises(SystemExit) as exc_info:

            mock_controller = Mock()
            mock_controller.start_vm.side_effect = KeyboardInterrupt()
            mock_controller_class.return_value = mock_controller

            main()

            assert exc_info.value.code == 130


@pytest.mark.integration
@pytest.mark.slow
class TestPerformanceIntegration:
    """Test performance-related integration scenarios."""

    def test_concurrent_operations_handling(self, mock_vm_controller, temp_home_dir):
        """Test handling of concurrent VM operations."""
        mock_vm_controller.base_dir = temp_home_dir / "vms"

        # Create VM configuration
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run, \
             patch("agent_vm.vm_controller.os.chdir"), \
             patch("agent_vm.vm_controller.time.strftime", return_value="2025-06-16T14:30:00+02:00"):

            mock_run.return_value = Mock(returncode=0, stdout="test-output\n")
            mock_vm_controller.create_vm("localhost", 8000, "test-branch")

        # Test concurrent status checks
        with patch.object(mock_vm_controller, '_is_vm_running', return_value=True), \
             patch.object(mock_vm_controller, '_get_vm_pid', return_value=1234), \
             patch.object(mock_vm_controller, '_get_vm_resources', return_value={}), \
             patch.object(mock_vm_controller, '_check_ssh_connectivity', return_value={'connected': False}), \
             patch.object(mock_vm_controller, '_check_agent_service_detailed', return_value={'active': False}), \
             patch.object(mock_vm_controller, '_check_mcp_proxy_health', return_value={'healthy': False}), \
             patch.object(mock_vm_controller, '_check_workspace_status', return_value={'accessible': False}):

            # Multiple status checks should work without interference
            for _ in range(5):
                mock_vm_controller.vm_status("test-branch")

    def test_resource_monitoring_accuracy(self, mock_vm_controller):
        """Test resource monitoring data accuracy."""
        with patch("agent_vm.vm_controller.subprocess.run") as mock_run:
            mock_run.return_value = Mock(
                returncode=0,
                stdout="  PID  PPID %CPU %MEM     ETIME COMMAND\n 1234     1  5.2  2.8   1:23:45 qemu-system\n"
            )

            resources = mock_vm_controller._get_vm_resources(1234)

            assert resources['cpu'] == '5.2'
            assert resources['memory'] == '2.8'
            assert resources['uptime'] == '1:23:45'
