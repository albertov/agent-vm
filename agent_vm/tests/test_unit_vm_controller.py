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
