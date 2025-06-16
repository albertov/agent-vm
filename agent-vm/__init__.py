"""
Agent VM - VM control tool for managing development VMs.

This package provides comprehensive VM lifecycle management including creation,
startup, shutdown, and workspace management for agent development environments.
"""

from .vm_controller import main

__version__ = "1.0.0"
__all__ = ["main"]
