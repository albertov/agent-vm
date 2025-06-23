# Agent VM - Secure MCP Agent Isolation System

Agent VM is a comprehensive system for running MCP (Model Context Protocol) agents in secure, isolated QEMU virtual machines. It provides enhanced security through hardware virtualization while maintaining full development workflow compatibility.

## Project Structure Overview

Here's what each file in this project does:

### Core Components
- **`agent_vm/vm_controller.py`** - Main Python application providing VM lifecycle management CLI
- **`vm-config.nix`** - NixOS VM configuration with virtualization settings, user setup, and security hardening
- **`agent-service.nix`** - NixOS module defining the systemd service for running MCP agents with minimal privileges
- **`flake.nix`** - Nix flake providing reproducible build environment and package definitions
- **`overlay.nix`** - Nix overlay defining custom packages including agent-vm and MCP tools

### Configuration & Build
- **`agent_vm/pyproject.toml`** - Python package configuration with dependencies and test setup
- **`codemcp.toml`** - CodeMCP tool configuration for development workflow integration
- **`selenium-server.nix`** - Selenium server configuration for web automation

### Testing & Scripts
- **`integration-test.py`** - Comprehensive integration tests that validate end-to-end VM workflows
- **`agent_vm/tests/`** - Unit and integration test suites with pytest configuration
- **Shell scripts** (`*.sh`) - Helper scripts for MCP server startup and system integration

### Documentation
- **`TODO.md`** - Development roadmap and implementation status tracking
- **`CHANGELOG.md`** - Version history and change documentation

## Key Benefits

**🔒 Complete Hardware Isolation**: Each agent runs in a separate QEMU VM with its own kernel instance, providing hypervisor-level security boundaries that prevent privilege escalation and memory access between agents.

**🚀 High Performance**: VirtioFS provides near-native filesystem performance for workspace sharing, while optimized QEMU configurations minimize overhead.

**🔧 Development-Friendly**: SSH access, comprehensive monitoring, and seamless integration with existing development workflows ensure no disruption to productivity.

**📦 Reproducible**: NixOS-based VMs ensure consistent, reproducible environments across different host systems.

**🛡️ Security Hardened**: Systemd security features, minimal capabilities, and network isolation reduce attack surface.

## Quick Start

### Prerequisites

- Linux system with KVM/QEMU virtualization support
- Nix package manager with flakes enabled
- Git for workspace management

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd agent-vm

# Build the agent-vm tool
nix build .#agent-vm

# Or run directly from flake
nix run .#agent-vm -- --help
```

### Basic Usage

```bash
# Create a new VM configuration for current branch
agent-vm create

# Start the VM with agent services
agent-vm start

# Check VM and agent status
agent-vm status

# Open SSH shell in the VM
agent-vm shell

# View real-time agent service logs
agent-vm logs

# Stop the VM
agent-vm stop

# Clean up VM configuration
agent-vm destroy
```

### Advanced Usage

```bash
# Create VM for specific branch and port
agent-vm create --branch=feature-x --port=8001

# Use custom state directory for isolation
agent-vm --state-dir ./test-vms create --branch=test

# List all VM configurations
agent-vm list

# Restart VM services
agent-vm restart

# Get detailed status with resource monitoring
agent-vm status --verbose
```

## Architecture

### VM Infrastructure

**QEMU Virtualization**: Each agent runs in a dedicated QEMU VM with:
- 4GB RAM and 4 CPU cores (configurable)
- Headless operation for optimal performance
- Hardware-assisted isolation via KVM

**VirtioFS Workspace Sharing**: High-performance filesystem sharing allows seamless collaboration between host and guest:
- `/workspace` directory mounted with `mapped-xattr` security model
- Near-native filesystem performance
- Proper permission mapping between host and guest

**Network Isolation**: VMs run in isolated network environments with controlled access:
- SSH access on host port 2222 → guest port 22
- MCP proxy on host port 8000 → guest port 8000
- Firewall rules restricting unnecessary network access

### Agent Service Architecture

**Systemd Integration**: Agents run as hardened systemd services with:
- Minimal required capabilities and permissions
- Automatic restart on failure with exponential backoff
- Comprehensive resource limits and security features
- Health monitoring and status reporting

**MCP Tools Integration**: Full MCP ecosystem available in VMs:
- `codemcp` - Core MCP functionality for code development
- `mcp-proxy` - HTTP proxy for MCP server communication
- `mcp-language-server` - Language server protocol integration
- `rescript-lsp` - ReScript language support
- `mcp-selenium` - Web automation capabilities
- `mcp-nixos` - NixOS configuration management

### Security Features

**Hardware Isolation**: VMs provide complete isolation through:
- Separate kernel instances preventing privilege escalation
- Hardware-assisted memory management unit (MMU) protection
- Hypervisor security boundary enforcement

**Systemd Security Hardening**:
- `NoNewPrivileges=true` - Prevents privilege escalation
- `ProtectSystem=strict` - Read-only filesystem protection
- `ProtectHome=true` - Home directory access restriction
- `PrivateNetwork=false` - Controlled network access for MCP communication
- `ReadWritePaths=["/workspace"]` - Minimal filesystem write access

**SSH Key Management**:
- Unique SSH keypairs generated per VM instance
- Ed25519 keys with secure permissions (600/644)
- No password authentication - key-based access only

## VM Lifecycle Management

### VM Creation

The `agent-vm create` command:

1. **Configuration Setup**: Creates VM configuration directory in `~/.local/share/agent-vms/<branch>/`
2. **SSH Key Generation**: Generates unique Ed25519 keypair for secure VM access
3. **Workspace Clone**: Clones current repository branch to VM workspace
4. **Git Remote Setup**: Configures origin and upstream remotes for collaboration
5. **Metadata Storage**: Saves VM configuration as JSON for persistence

### VM Startup

The `agent-vm start` command:

1. **Configuration Validation**: Verifies VM configuration exists and is valid
2. **VM Building**: Uses Nix to build VM with injected SSH keys and workspace paths
3. **Process Launch**: Starts QEMU VM process with proper naming and resource allocation
4. **SSH Readiness**: Waits for SSH connectivity with retry logic and timeouts
5. **Agent Service Startup**: Starts systemd agent service in VM
6. **Health Verification**: Confirms MCP proxy is responding and healthy

### Status Monitoring

The `agent-vm status` command provides comprehensive monitoring:

**VM Process Status**:
- Process existence and PID tracking
- Resource usage (CPU, memory, uptime)
- QEMU process health monitoring

**SSH Connectivity**:
- SSH connection testing with detailed diagnostics
- Authentication verification
- Network connectivity validation

**Agent Service Status**:
- Systemd service state (active, failed, restarting)
- Process ID and restart count tracking
- Memory usage and performance metrics

**MCP Proxy Health**:
- HTTP endpoint health checks
- Response time measurement
- Service availability verification

**Workspace Status**:
- Directory accessibility and permissions
- Git repository status and cleanliness
- Disk usage and workspace size

## Configuration

### VM Configuration (`vm-config.nix`)

The VM configuration defines:

```nix
{
  # Virtualization settings
  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;      # 4GB RAM
      cores = 4;              # 4 CPU cores
      diskSize = 4096;        # 4GB disk
      graphics = false;       # Headless mode

      # VirtioFS workspace sharing
      sharedDirectories.workspace = {
        source = "/workspace";
        target = "/workspace";
        securityModel = "mapped-xattr";
      };

      # Network port forwarding
      forwardPorts = [
        { from = "host"; host.port = 8000; guest.port = 8000; }  # MCP
        { from = "host"; host.port = 2222; guest.port = 22; }    # SSH
      ];
    };
  };

  # Agent service configuration
  services.agent-mcp = {
    enable = true;
    user = "dev";
    group = "dev";
    workspaceDir = "/workspace";
    port = 8000;
    allowOrigin = "https://claude.ai";
  };
}
```

### Agent Service (`agent-service.nix`)

The systemd service configuration provides:

```nix
{
  systemd.services.agent-mcp = {
    description = "Agent MCP Service";
    serviceConfig = {
      # Security hardening
      NoNewPrivileges = true;
      ProtectSystem = "strict";
      ProtectHome = true;
      ReadWritePaths = [ "/workspace" ];

      # Process management
      Type = "exec";
      User = "dev";
      Group = "dev";
      Restart = "always";
      RestartSec = "5s";

      # Health monitoring
      ExecStartPost = "health-check-script";
    };
  };
}
```

### State Directory Structure

VM configurations are stored in `~/.local/share/agent-vms/<branch>/`:

```
~/.local/share/agent-vms/
├── main/
│   ├── config.json          # VM metadata and configuration
│   ├── ssh/
│   │   ├── id_ed25519       # Private SSH key (600)
│   │   └── id_ed25519.pub   # Public SSH key (644)
│   ├── workspace/           # Git repository workspace
│   └── vm.pid              # VM process ID (when running)
└── feature-branch/
    └── ...                  # Similar structure for other branches
```

## Testing

### Unit Tests

Run focused unit tests for individual components:

```bash
# Run all unit tests
pytest -m unit

# Run specific test files
pytest agent_vm/tests/test_unit_vm_controller.py

# Run with coverage
pytest --cov=agent_vm -m unit
```

### Integration Tests

Run comprehensive integration tests that validate end-to-end workflows:

```bash
# Run all integration tests
nix run .#integration-test

# Run specific test by name
nix run .#integration-test -- -k test_vm_creation

# Run with debug output
nix run .#integration-test -- --debug -s

# Stop on first failure
nix run .#integration-test -- -x

# Custom timeout and verbose output
nix run .#integration-test -- --timeout 180 --verbose

# Combine multiple options
nix run .#integration-test -- --debug -k test_agent_service -s

# Pass additional pytest arguments after --
nix run .#integration-test -- -- -v --tb=long --maxfail=2
```

**Integration Test CLI Options**:
- `--agent-vm PATH` - Path to agent-vm executable (default: agent-vm in PATH)
- `--timeout SECONDS` - Timeout for VM operations (default: 120)
- `--debug` - Enable debug mode with full output
- `--verbose` - Enable verbose logging
- `-k EXPRESSION` - Run tests matching expression (pytest -k)
- `-s` - Show print statements (pytest -s)
- `-x` - Stop on first failure (pytest -x)
- `--` - All arguments after this are passed to pytest

**Examples**:
```bash
# Quick test of VM creation
integration-test -k test_vm_creation

# Debug a failing test with output
integration-test --debug -s -k test_agent_service

# Run all tests with custom timeout
integration-test --timeout 180

# Run with multiple pytest options
integration-test -- -k "test_vm or test_agent" -v --maxfail=1
```

### Test Categories

Tests are organized with pytest markers:

- **`unit`** - Fast unit tests for individual methods
- **`integration`** - End-to-end workflow testing
- **`vm`** - Tests requiring VM functionality (skipped without virtualization)
- **`ssh`** - Tests requiring SSH connectivity
- **`slow`** - Long-running tests (excluded by default)

### Integration Test Features

The integration test executable (`integration-test.py`) provides:

**CLI-Only Testing**: Calls `agent-vm` through command line exclusively (no mocks) for realistic testing

**Isolated Environments**: Uses temporary state directories to prevent test interference

**Comprehensive Coverage**: Tests complete workflows from VM creation to destruction

**Debug Mode**: Captures and logs all subprocess stderr/stdout for troubleshooting

**Virtualization Detection**: Automatically skips VM tests when nested virtualization is unavailable

## Development

### Development Environment

Set up the development environment:

```bash
# Enter development shell with all tools
nix develop

# Run formatters
nix run .#format

# Run all tests
nix run .#test

# Run integration tests
nix run .#integration-test
```

### Adding New Features

1. **Follow existing patterns**: Study `vm_controller.py` for CLI command patterns
2. **Add comprehensive tests**: Include both unit and integration tests
3. **Update documentation**: Keep README and docstrings current
4. **Test security**: Verify new features don't compromise isolation
5. **Performance check**: Ensure changes don't significantly impact VM startup time

### Code Style

- **Type Hints**: All functions should have proper type annotations
- **Error Handling**: Comprehensive error handling with user-friendly messages
- **Logging**: Use structured logging with appropriate levels
- **Documentation**: Docstrings for all public methods and classes

## Host System Requirements

### Virtualization Support

The system requires nested virtualization support to run QEMU VMs:

#### NixOS Host Configuration

```nix
# /etc/nixos/configuration.nix
{
  # Enable nested virtualization
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_amd nested=1
  '';

  # Required virtualization services
  virtualisation = {
    libvirtd.enable = true;
    qemu.package = pkgs.qemu_kvm;
  };

  # User permissions
  users.users.developer = {
    extraGroups = [ "libvirtd" "kvm" ];
  };

  # Kernel modules
  boot.kernelModules = [ "kvm-intel" "kvm-amd" ];
}
```

#### Other Linux Distributions

```bash
# Ubuntu/Debian
sudo apt install qemu-kvm libvirt-daemon-system bridge-utils
sudo usermod -aG libvirt,kvm $USER

# RedHat/CentOS/Fedora
sudo dnf install qemu-kvm libvirt bridge-utils
sudo usermod -aG libvirt,kvm $USER

# Enable nested virtualization
echo 'options kvm_intel nested=1' | sudo tee /etc/modprobe.d/kvm.conf
echo 'options kvm_amd nested=1' | sudo tee -a /etc/modprobe.d/kvm.conf
sudo systemctl enable --now libvirtd
```

## Troubleshooting

### Common Issues

**VM Startup Failures**:
- Check virtualization support: `lscpu | grep Virtualization`
- Verify KVM modules: `lsmod | grep kvm`
- Check permissions: `groups $USER` should include `kvm` and `libvirt`

**SSH Connection Issues**:
- Use debug mode: `agent-vm --verbose start`
- Check SSH key permissions in `~/.local/share/agent-vms/<branch>/ssh/`
- Verify VM is running: `agent-vm status`

**Agent Service Failures**:
- Check logs: `agent-vm logs`
- SSH into VM: `agent-vm shell` then `journalctl -u agent-mcp`
- Verify workspace accessibility: Check `/workspace` permissions in VM

**Performance Issues**:
- Monitor resource usage: `agent-vm status` shows CPU/memory
- Check host system resources: VMs require significant RAM/CPU
- Verify VirtioFS performance: Large file operations should be near-native speed

### Debug Mode

Use debug mode for detailed troubleshooting:

```bash
# Enable verbose logging
agent-vm --verbose start

# Integration test debug mode
./integration-test.py --debug --timeout 30

# Check detailed VM status
agent-vm status --verbose
```

## Security Considerations

### Isolation Boundaries

**Hardware Isolation**: QEMU provides complete isolation through:
- Separate virtual machines with independent kernel instances
- Hardware-assisted memory management preventing cross-VM access
- Hypervisor security boundary enforcement

**Network Isolation**: VMs operate in isolated network environments:
- Only specified ports (SSH, MCP) are accessible from host
- No direct network access between VMs
- Firewall rules enforce minimal connectivity

**Filesystem Isolation**: Workspace sharing is controlled and limited:
- Only `/workspace` directory is shared between host and guest
- VirtioFS uses `mapped-xattr` for proper permission mapping
- No access to host filesystem outside shared directories

### Security Best Practices

1. **Regular Updates**: Keep NixOS and QEMU updated for security patches
2. **Minimal Exposure**: Only forward necessary ports and limit network access
3. **Key Management**: SSH keys are unique per VM and properly secured
4. **Service Hardening**: Systemd security features minimize attack surface
5. **Resource Limits**: VMs have defined resource limits preventing resource exhaustion

### Threat Model

**Protected Against**:
- Agent code execution escaping to host system
- Cross-agent contamination and data access
- Privilege escalation within VM environment
- Network-based attacks between agents

**Not Protected Against**:
- Host system compromise (VMs depend on host security)
- Side-channel attacks (timing, power analysis)
- Hypervisor vulnerabilities (depends on QEMU security)
- Physical access to host system

## Contributing

1. **Install development dependencies**: `nix develop`
2. **Run tests**: `pytest` and `./integration-test.py`
3. **Follow code style**: Use type hints and comprehensive error handling
4. **Add tests**: Include both unit and integration tests for new features
5. **Update documentation**: Keep README and code comments current

## License

MIT License - see LICENSE file for details.
