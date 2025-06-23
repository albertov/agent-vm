"""
Main entry point for agent-vm command-line interface.

This module contains all the CLI command definitions and the main entry point.
The core VM management functionality is in vm_controller.py.
"""

import subprocess
import sys
from typing import Optional

import typer

from agent_vm.vm_controller import VMController, setup_logging, logger

# Global state for options
_global_state = {
    "state_dir": None,
    "verbose": False,
    "debug": False,
    "timeout": 120  # Default timeout in seconds
}

# Initialize typer app
app = typer.Typer(
    name="agent-vm",
    help="VM control command for managing development VMs",
    epilog="""
Examples:
  agent-vm create --branch=feature-x --port=8001
  agent-vm start feature-x
  agent-vm restart feature-x
  agent-vm shell feature-x
  agent-vm list
  agent-vm destroy feature-x
    """
)


def _setup_global_options(
    state_dir: Optional[str] = typer.Option(None, help="Override default state directory"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose logging"),
    debug: bool = typer.Option(False, "--debug", "-d", help="Enable debug logging (more verbose than --verbose)"),
    timeout: int = typer.Option(120, "--timeout", "-t", help="Global timeout in seconds for VM operations")
) -> None:
    """Set up global options that apply to all commands."""
    from agent_vm.vm_controller import _global_debug

    _global_state["state_dir"] = state_dir
    _global_state["verbose"] = verbose
    _global_state["debug"] = debug
    _global_state["timeout"] = timeout

    # Set global debug flag for subprocess logging
    # We need to update the module-level variable in vm_controller
    import agent_vm.vm_controller as vm_controller_module
    vm_controller_module._global_debug = debug

    # Debug takes precedence over verbose
    if debug:
        setup_logging(verbose=True)
        logger.debug("Debug mode enabled - maximum verbosity for all commands")
    elif verbose:
        setup_logging(verbose=True)
        logger.debug("Verbose mode enabled for all commands")
    else:
        setup_logging(verbose=False)


@app.callback()
def main_callback(
    state_dir: Optional[str] = typer.Option(None, help="Override default state directory"),
    verbose: bool = typer.Option(False, "--verbose", "-v", help="Enable verbose logging"),
    debug: bool = typer.Option(False, "--debug", "-d", help="Enable debug logging (more verbose than --verbose)"),
    timeout: int = typer.Option(120, "--timeout", "-t", help="Global timeout in seconds for VM operations")
) -> None:
    """Main callback to handle global options."""
    _setup_global_options(state_dir, verbose, debug, timeout)


@app.command()
def create(
    host: str = typer.Option("localhost", help="Host to bind VM ports to"),
    port: int = typer.Option(8000, help="Port for MCP proxy forwarding"),
    branch: Optional[str] = typer.Option(None, help="Branch name for VM (default: current branch)"),
    config: str = typer.Option("vm-config.nix", help="Path to VM NixOS config")
) -> None:
    """Create a new VM configuration."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.create_vm(host, port, branch, config)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def start(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Start VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.start_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def stop(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Stop VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.stop_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def restart(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Restart VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.restart_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def status(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Show VM status for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.vm_status(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def shell(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Open SSH shell in VM for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.vm_shell(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def logs(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Show VM logs for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.vm_logs(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command("list")
def list_vms() -> None:
    """List all VM configurations."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.list_vms()
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


@app.command()
def destroy(
    branch: Optional[str] = typer.Argument(None, help="Branch name (default: current branch)")
) -> None:
    """Destroy VM configuration for branch."""
    controller = VMController(state_dir=_global_state["state_dir"], timeout=_global_state["timeout"])

    try:
        controller.destroy_vm(branch)
    except subprocess.CalledProcessError as e:
        logger.error(f"Command failed with exit code {e.returncode}")
        raise typer.Exit(e.returncode)
    except KeyboardInterrupt:
        logger.info("Operation cancelled by user")
        raise typer.Exit(130)
    except Exception as e:
        logger.error(f"Unexpected error: {e}")
        raise typer.Exit(1)


def main() -> None:
    """Main entry point for agent-vm command."""
    app()


if __name__ == "__main__":
    main()
