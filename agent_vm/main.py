# TODO: Move the rest of the typer functions for the CLI from
# vm_controller.py here

from agent_vm.vm_controller import app

def main() -> None:
    """Main entry point for agent-vm command."""
    app()


if __name__ == "__main__":
    main()
