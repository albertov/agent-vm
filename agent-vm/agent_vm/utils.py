#!/usr/bin/env python3
"""
Common utilities for agent-vm.

This module contains shared logging, process management, and other utility functions
used across the agent-vm codebase.
"""

import atexit
import logging
import os
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, List, Optional, Union


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


class ColorFormatter(logging.Formatter):
    """Custom formatter with color support."""

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
            if "✅" in record.msg or "SUCCESS" in record.msg.upper():
                formatted = Colors.BRIGHT_GREEN + "✅ " + Colors.RESET + formatted
            elif "❌" in record.msg or "ERROR" in record.msg.upper():
                formatted = Colors.BRIGHT_RED + "❌ " + Colors.RESET + formatted
            elif "⚠️" in record.msg or "WARNING" in record.msg.upper():
                formatted = Colors.BRIGHT_YELLOW + "⚠️ " + Colors.RESET + formatted
            elif "🔧" in record.msg or "BUILDING" in record.msg.upper():
                formatted = Colors.BRIGHT_CYAN + "🔧 " + Colors.RESET + formatted
            elif "🚀" in record.msg or "STARTING" in record.msg.upper():
                formatted = Colors.BRIGHT_MAGENTA + "🚀 " + Colors.RESET + formatted

        return formatted


def setup_logging(verbose: bool = False, logger_name: Optional[str] = None) -> logging.Logger:
    """
    Set up colored logging configuration.

    Args:
        verbose: Enable debug logging if True
        logger_name: Name of the logger to configure (default: root logger)

    Returns:
        Configured logger instance
    """
    level = logging.DEBUG if verbose else logging.INFO

    # Get or create logger
    logger = logging.getLogger(logger_name) if logger_name else logging.getLogger()

    # Create formatter
    formatter = ColorFormatter(
        fmt="%(asctime)s [%(levelname)s] %(message)s",
        datefmt="%H:%M:%S"
    )

    # Create handler
    handler = logging.StreamHandler(sys.stdout)
    handler.setFormatter(formatter)

    # Configure logger
    logger.setLevel(level)
    logger.handlers = [handler]
    logger.propagate = False

    return logger


# Global list to track temp files for cleanup
_temp_files: List[tempfile._TemporaryFileWrapper] = []


def _cleanup_tempfiles():
    """Clean up any remaining tempfiles on exit."""
    global _temp_files
    for temp_file in _temp_files:
        try:
            if hasattr(temp_file, 'name') and os.path.exists(temp_file.name):
                os.unlink(temp_file.name)
        except Exception:
            pass  # Ignore errors during cleanup
    _temp_files.clear()


def cleanup_old_logs(state_dir: Path, max_age_days: int = 7):
    """
    Clean up old log files from state directory.

    Args:
        state_dir: Base state directory containing VM directories
        max_age_days: Remove logs older than this many days
    """
    if not state_dir or not state_dir.exists():
        return

    logger = logging.getLogger(__name__)

    try:
        import time
        cutoff_time = time.time() - (max_age_days * 24 * 60 * 60)

        # Clean up logs from VM-specific directories: {state_dir}/{vm_name}/tmp/logs/
        for vm_dir in state_dir.iterdir():
            if vm_dir.is_dir():
                vm_logs_dir = vm_dir / "tmp" / "logs"
                if vm_logs_dir.exists():
                    for log_file in vm_logs_dir.rglob("*.log"):
                        if log_file.is_file():
                            try:
                                if log_file.stat().st_mtime < cutoff_time:
                                    log_file.unlink()
                                    logger.debug(f"Cleaned up old log file: {log_file}")
                            except (OSError, PermissionError):
                                pass  # Ignore cleanup errors

                    # Remove empty tmp/logs directory
                    try:
                        if vm_logs_dir.exists() and not any(vm_logs_dir.iterdir()):
                            vm_logs_dir.rmdir()
                            # Also remove tmp directory if empty
                            tmp_dir = vm_logs_dir.parent
                            if tmp_dir.exists() and not any(tmp_dir.iterdir()):
                                tmp_dir.rmdir()
                    except OSError:
                        pass  # Directory not empty or other error

        # Also clean up any legacy logs in {state_dir}/tmp/logs/ (from old structure)
        legacy_logs_dir = state_dir / "tmp" / "logs"
        if legacy_logs_dir.exists():
            for log_file in legacy_logs_dir.rglob("*.log"):
                if log_file.is_file():
                    try:
                        if log_file.stat().st_mtime < cutoff_time:
                            log_file.unlink()
                            logger.debug(f"Cleaned up legacy log file: {log_file}")
                    except (OSError, PermissionError):
                        pass  # Ignore cleanup errors

            # Clean up empty legacy directories
            try:
                for subdir in legacy_logs_dir.iterdir():
                    if subdir.is_dir():
                        try:
                            subdir.rmdir()  # Remove if empty
                        except OSError:
                            pass
                if not any(legacy_logs_dir.iterdir()):
                    legacy_logs_dir.rmdir()
            except OSError:
                pass

    except Exception:
        pass  # Ignore all cleanup errors


def get_last_lines(file_path: Union[str, Path], num_lines: int = 20) -> List[str]:
    """
    Get the last N lines from a file.

    Args:
        file_path: Path to the file to read
        num_lines: Number of lines to retrieve from the end

    Returns:
        List of last N lines from the file
    """
    logger = logging.getLogger(__name__)

    try:
        with open(file_path, 'r', encoding='utf-8', errors='replace') as f:
            lines = f.readlines()
            return lines[-num_lines:] if len(lines) > num_lines else lines
    except Exception as e:
        logger.warning(f"Failed to read log file {file_path}: {e}")
        return []


class ProcessWithOutput:
    """
    Wrapper for subprocess.Popen that captures output to tempfiles.

    This class provides a convenient way to run subprocesses while capturing
    their output to temporary files for debugging and logging purposes.
    """

    #TODO: I want types!!! ffs!
    def __init__(self, cmd: List[str], state_dir: Optional[Path] = None,
                 vm_name: Optional[str] = None, debug: bool = False, **kwargs):
        """
        Initialize Popen with tempfile capture.

        Args:
            cmd: Command to run as list of strings
            state_dir: VM state directory for log storage
            vm_name: VM name for log organization
            debug: Whether to keep logs for debugging
            **kwargs: Additional keyword arguments for subprocess.Popen
        """
        logger = logging.getLogger(__name__)

        # Determine log directory - use VM-specific structure when possible
        if state_dir:
            # For VM operations: {state_dir}/tmp/logs/ where state_dir is {base_dir}/{vm_name}
            # For general operations: {state_dir}/tmp/logs/
            log_dir = state_dir / "tmp" / "logs"
            log_dir.mkdir(parents=True, exist_ok=True)

            if vm_name:
                log_prefix = f"{vm_name}_"
            else:
                log_prefix = "agent_vm_"
        else:
            # Fallback to system tmp
            log_dir = Path("/tmp")
            log_prefix = "agent_vm_"

        # Create temporary files for stdout and stderr
        self.stdout_temp = tempfile.NamedTemporaryFile(mode='w+', prefix=f'{log_prefix}stdout_',
                                                       suffix='.log', delete=False, dir=log_dir)
        self.stderr_temp = tempfile.NamedTemporaryFile(mode='w+', prefix=f'{log_prefix}stderr_',
                                                       suffix='.log', delete=False, dir=log_dir)

        # Track for cleanup
        _temp_files.extend([self.stdout_temp, self.stderr_temp])

        # Set capture output options if not already set
        kwargs_copy = kwargs.copy()
        # Remove parameters that are for subprocess.run, not Popen
        kwargs_copy.pop('capture_output', None)
        self.check = kwargs_copy.pop('check', True)  # Save for later use
        kwargs_copy.pop('timeout', None)  # timeout is handled in wait()
        if 'stdout' not in kwargs_copy:
            kwargs_copy['stdout'] = self.stdout_temp
        if 'stderr' not in kwargs_copy:
            kwargs_copy['stderr'] = self.stderr_temp
        if 'text' not in kwargs_copy:
            kwargs_copy['text'] = True

        logger.debug(f"Starting process: {' '.join(cmd)}")
        if debug:
            logger.debug(f"stdout log: {self.stdout_temp.name}")
            logger.debug(f"stderr log: {self.stderr_temp.name}")

        self.cmd = cmd
        self.debug = debug
        self.process = subprocess.Popen(cmd, **kwargs_copy)
        self.pid = self.process.pid

    def wait(self, timeout=None):
        """Wait for process to complete."""
        logger = logging.getLogger(__name__)

        try:
            returncode = self.process.wait(timeout)

            # Flush temp files
            self.stdout_temp.flush()
            self.stderr_temp.flush()

            # If process failed, log output
            if returncode != 0:
                logger.error(f"Process failed with return code {returncode}: {' '.join(self.cmd)}")

                if self.debug:
                    logger.error(f"stdout log path: {self.stdout_temp.name}")
                    logger.error(f"stderr log path: {self.stderr_temp.name}")

                # Log last 20 lines of output
                stdout_lines = get_last_lines(self.stdout_temp.name, 20)
                if stdout_lines:
                    logger.error("Last 20 lines of stdout:")
                    for line in stdout_lines:
                        logger.error(f"stdout: {line.rstrip()}")

                stderr_lines = get_last_lines(self.stderr_temp.name, 20)
                if stderr_lines:
                    logger.error("Last 20 lines of stderr:")
                    for line in stderr_lines:
                        logger.error(f"stderr: {line.rstrip()}")
            else:
                # Command succeeded - don't clean up logs here, leave it for cleanup()
                pass

            return returncode

        except subprocess.TimeoutExpired:
            logger.error(f"Process timed out: {' '.join(self.cmd)}")
            self.terminate()
            raise

    def terminate(self):
        """Terminate the process."""
        self.process.terminate()

    def kill(self):
        """Kill the process."""
        self.process.kill()

    def cleanup(self):
        """Clean up temporary files."""
        try:
            self.stdout_temp.close()
            os.unlink(self.stdout_temp.name)
        except OSError:
            pass

        try:
            self.stderr_temp.close()
            os.unlink(self.stderr_temp.name)
        except OSError:
            pass

    def __enter__(self):
        """Context manager entry."""
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        """Context manager exit - ensure cleanup."""
        if self.process.poll() is None:
            self.terminate()
            try:
                self.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.kill()

        if not self.debug:
            self.cleanup()


def run_subprocess(cmd: List[str], state_dir: Optional[Path] = None,
                   vm_name: Optional[str] = None, debug: bool = False,
                   **kwargs) -> subprocess.CompletedProcess:
    """
    Run subprocess with stdout/stderr captured to tempfiles in state directory.

    This is a convenience function that uses ProcessWithOutput internally to
    run a subprocess and return a CompletedProcess result.

    Args:
        cmd: Command to run as list of strings
        state_dir: VM state directory (should be {base_dir}/{vm_name} for VM operations)
        vm_name: VM name for log organization
        debug: Whether to keep logs for debugging
        **kwargs: Additional keyword arguments for subprocess.run

    Returns:
        CompletedProcess with stdout/stderr captured

    On error, logs the last 20 lines of stdout/stderr and their file paths.
    """
    logger = logging.getLogger(__name__)

    # Use ProcessWithOutput to handle the subprocess
    with ProcessWithOutput(cmd, state_dir=state_dir, vm_name=vm_name,
                          debug=debug, **kwargs) as proc:
        try:
            # Wait for completion
            timeout = kwargs.get('timeout')
            returncode = proc.wait(timeout=timeout)

            # Read captured output
            proc.stdout_temp.seek(0)
            stdout = proc.stdout_temp.read()

            proc.stderr_temp.seek(0)
            stderr = proc.stderr_temp.read()

            # Create CompletedProcess result
            result = subprocess.CompletedProcess(
                args=cmd,
                returncode=returncode,
                stdout=stdout,
                stderr=stderr
            )

            # Check if we should raise on error
            if proc.check and returncode != 0:
                raise subprocess.CalledProcessError(
                    returncode, cmd, output=stdout, stderr=stderr
                )

            return result

        except subprocess.TimeoutExpired:
            # Read any captured output before timeout
            proc.stdout_temp.seek(0)
            stdout = proc.stdout_temp.read()

            proc.stderr_temp.seek(0)
            stderr = proc.stderr_temp.read()

            raise subprocess.TimeoutExpired(cmd, timeout, output=stdout, stderr=stderr)


# Register cleanup function to run at exit
atexit.register(_cleanup_tempfiles)
