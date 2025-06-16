# Agent VM - VM Control Tool

Agent VM is a comprehensive VM lifecycle management tool for development environments that provides secure agent isolation using QEMU virtualization.

## Features

- **VM Lifecycle Management**: Create, start, stop, restart, and destroy VMs
- **SSH Key Management**: Automatic SSH key generation and secure access
- **Workspace Isolation**: Isolated development workspaces per VM
- **Multi-instance Support**: Multiple VMs per git branch
- **Comprehensive Monitoring**: VM status, resource usage, and health checks
- **Agent Service Integration**: Seamless MCP (Model Context Protocol) agent service management

## Installation

This package requires Python 3.8+ and is designed to work with Nix environments.

```bash
# Install in development mode
pip install -e .

# Install with test dependencies
pip install -e ".[test]"
```

## Quick Start

```bash
# Create a new VM configuration for current branch
agent-vm create

# Start the VM
agent-vm start

# Check VM status
agent-vm status

# Open SSH shell in VM
agent-vm shell

# View VM logs
agent-vm logs

# Stop the VM
agent-vm stop

# Clean up VM configuration
agent-vm destroy
```

## Usage

### VM Management

```bash
# Create VM for specific branch and port
agent-vm create --branch=feature-x --port=8001

# Start VM for specific branch
agent-vm start feature-x

# List all VM configurations
agent-vm list

# Restart VM
agent-vm restart feature-x

# Destroy VM configuration
agent-vm destroy feature-x
```

### Monitoring

```bash
# Get detailed VM status
agent-vm status

# Monitor VM logs in real-time
agent-vm logs

# Check specific branch
agent-vm status feature-x
```

## Architecture

Agent VM uses QEMU virtualization with:

- **NixOS VMs**: Reproducible, secure VM configurations
- **VirtioFS**: High-performance workspace sharing
- **SSH Access**: Secure shell access with auto-generated keys
- **Systemd Integration**: Agent services managed by systemd
- **Resource Monitoring**: CPU, memory, and disk usage tracking

## Configuration

VM configurations are stored in `~/.local/share/agent-vms/<branch>/`:

- `config.json`: VM metadata and configuration
- `ssh/`: SSH keys for VM access
- `workspace/`: Git repository workspace
- `vm.pid`: VM process ID file

## Security

- Each VM uses unique SSH keypairs
- VMs run in isolated QEMU processes
- Minimal attack surface with security-hardened systemd services
- Workspace isolation prevents cross-contamination

## Testing

Run the comprehensive test suite:

```bash
# Run all tests
pytest

# Run with coverage
pytest --cov=agent_vm

# Run specific test categories
pytest -m unit
pytest -m integration
pytest -m "not slow"

# Run VM-related tests (requires VM capabilities)
pytest -m vm

# Run SSH connectivity tests
pytest -m ssh
```

## Development

This project uses:

- **Python 3.8+** with type hints
- **pytest** for testing with extensive markers
- **Nix** for reproducible builds
- **setuptools** with pyproject.toml configuration

## Contributing

1. Install development dependencies: `pip install -e ".[test]"`
2. Run tests: `pytest`
3. Follow existing code style and type hints
4. Add tests for new functionality

## License

MIT License - see LICENSE file for details.
