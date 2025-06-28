# Agent VM - Secure MCP Agent Isolation System

Agent VM is a comprehensive system for running MCP (Model Context Protocol) agents in secure, isolated QEMU virtual machines. It provides enhanced security through hardware virtualization while maintaining full development workflow compatibility.

All VMs state is located in an user configurable `--state-dir` directory
(defaults to `~/.local/share/agent-vms/<repo>-<branch>`)

## Key Benefits

**🔒 Complete Hardware Isolation**: Each agent runs in a separate QEMU VM with its own kernel instance, providing hypervisor-level security boundaries that prevent privilege escalation and memory access between agents.

**🚀 High Performance**: VirtioFS provides near-native filesystem performance for workspace sharing, while optimized QEMU configurations minimize overhead.

**🔧 Development-Friendly**: Comprehensive monitoring and seamless integration with existing development workflows ensure no disruption to productivity.

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
# Create a new VM configuration for current directory
agent-vm create

# Create VM for specific workspace directory
agent-vm create /path/to/workspace

# Start the VM
agent-vm start

# Check VM status
agent-vm status

# Connect to VM shell via serial console
agent-vm shell

# Stop the VM
agent-vm stop

# Clean up VM configuration
agent-vm destroy
```

### Advanced Usage

```bash
# Create VM with custom configuration
agent-vm create --name=my-vm --memory-size=4 --cores=2 --port=8001

# Create ephemeral VM (tmpfs root filesystem)
agent-vm create --ephemeral --disk-size=10

# Use custom state directory
agent-vm --state-dir ./test-vms create

# List all VM configurations
agent-vm list

# Update existing VM configuration
agent-vm update --memory-size=8 --cores=4

# Reset VM (delete hard disk, keep configuration)
agent-vm reset

# Enable verbose or debug logging
agent-vm --verbose start
agent-vm --debug create
```

### VM Configuration Options

**Resource Configuration:**
- `--memory-size GB` / `-m` - Memory in GB
- `--cores CORES` / `-c` - Number of CPU cores
- `--disk-size GB` / `-S` - Disk size in GB
- `--ephemeral` / `-e` - Use tmpfs for root filesystem

**System Configuration:**
- `--port PORT` / `-p` - Port number
- `--uid UID` / `-u` - User ID
- `--gid GID` / `-g` - Group ID
- `--group GROUP` / `-G` - Group name This is needed when the gid you want to
  assign conflict with a gid inside the VM

**Development Configuration:**
- `--flake FLAKE` / `-F` - Flake path (defaults to HEAD of current directory)
- `--shell SHELL` / `-s` - Flake shell name attribute (defaults to default)
- `--base PATH` / `-b` - Nix base configuration file path
- `--system-packages PACKAGES` - System packages (comma-separated)
- `--additional-paths PATHS` - Additional Nix paths (comma-separated)

## Architecture

### VM Infrastructure

**Network Isolation**: VMs run in isolated network environments with controlled access:
- MCP proxy on host port 8000 → guest port 8000
- Firewall rules restricting unnecessary network access

### Agent Service Architecture

**Systemd Integration**: Agents run as hardened systemd services with:
- Minimal required capabilities and permissions
- Comprehensive resource limits and security features
- Health and status reporting

### Security Features

**Hardware Isolation**: VMs provide complete isolation through:
- Separate kernel instances preventing privilege escalation
- Hardware-assisted memory management unit (MMU) protection
- Hypervisor security boundary enforcement

**Systemd Security Hardening**:
- `NoNewPrivileges=true` - Prevents privilege escalation
- `ProtectSystem=strict` - Read-only filesystem protection
- `ProtectHome=true` - Home directory access restriction
- `ReadWritePaths=["/workspace"]` - Minimal filesystem write access

## VM Lifecycle Management

### VM Creation

The `agent-vm create [workspace]` command:

1. **Workspace Setup**: Uses current directory or specified workspace path
2. **Name Generation**: Auto-generates VM name from git repository and branch (or uses `--name`)
3. **Configuration Creation**: Creates VM configuration with specified or default settings
4. **State Directory**: Creates VM state directory structure

### VM Configuration Update

The `agent-vm update [workspace]` command:

1. **Configuration Loading**: Loads existing VM configuration
2. **Settings Merge**: Merges CLI options with existing configuration (CLI takes precedence)
3. **Configuration Update**: Updates VM configuration without removing disk image

### VM Startup

The `agent-vm start [--name VM_NAME]` command:

1. **Configuration Validation**: Verifies VM configuration exists and is valid
2. **VM Building**: Uses Nix to build VM with injected SSH keys and workspace paths
3. **Process Launch**: Starts QEMU VM process with proper naming and resource allocation

### VM Operations

**Status Monitoring**: `agent-vm status [--name VM_NAME]`
- VM process status and resource usage
- Configuration details and health checks

**Shell Access**: `agent-vm shell [--name VM_NAME]`
- Connects to VM via serial console
- Auto-detects VM name if not specified

**VM Control**:
- `agent-vm stop [--name VM_NAME]` - Stop running VM
- `agent-vm reset [--name VM_NAME]` - Delete VM disk image (keeps configuration)
- `agent-vm destroy [--name VM_NAME]` - Remove VM configuration completely

**VM Listing**: `agent-vm list`
- Shows all configured VMs and their status

## Configuration

### VM Configuration (`vm-config.nix`)

The VM configuration defines:

### Agent Service (`services/mcp-proxy.nix`)

### Global Options

**Logging Control:**
- `--verbose` / `-v` - Enable verbose logging (Debug level)
- `--debug` / `-d` - Enable debug logging (Trace level)

**State Management:**
- `--state-dir DIR` - Override default state directory

### VM Naming

VMs are automatically named based on git repository and branch:
- Repository name is extracted from git remote origin URL
- Branch name is detected from current git branch
- Final name format: `<repo-name>-<branch-name>` (with dashes converted to underscores)
- Override with `--name` option for custom naming

### State Directory Structure

VM configurations are stored in `~/.local/share/agent-vms/<vm-name>/` (or custom `--state-dir`):

```
~/.local/share/agent-vms/my-repo-main/
├── config.json          # VM metadata and configuration
├── ssh/
│   ├── id_ed25519       # Private SSH key (600)
│   └── id_ed25519.pub   # Public SSH key (644)
├── workspace/           # Git repository workspace
└── vm.pid              # VM process ID (when running)
```

## Testing

### Unit Tests

Run focused unit tests for individual components:

```bash
# Run all unit tests
cabal test

cabal test --test-options='--match="test name"'
```

### Integration Tests

Run comprehensive integration tests that validate end-to-end workflows:

```bash
# Run all integration tests
nix run .#integration-test

# Run with debug output
nix run .#integration-test -- --debug -s

# Custom timeout and verbose output
nix run .#integration-test -- --timeout 180 --verbose
```

**Integration Test CLI Options**:
- `--agent-vm PATH` - Path to agent-vm executable (default: agent-vm in PATH)
- `--timeout SECONDS` - Timeout for VM operations (default: 120)
- `--debug` - Enable debug mode with full output
- `--verbose` - Enable verbose logging
- `--` - All arguments after this are passed to the runner

## Development

### Development Environment

Set up the development environment:

```bash
# Enter development shell with all tools
nix develop

# Run formatters
nix run .#format

# Run integration tests
nix run .#integration-test
```

### Adding New Features

2. **Add comprehensive tests**: Include both unit and integration tests
3. **Update documentation**: Keep README and docstrings current
4. **Test security**: Verify new features don't compromise isolation

### Code Style

- **Type Hints**: All functions should have proper type annotations
- **Error Handling**: Comprehensive error handling with user-friendly messages
- **Logging**: Use structured logging with appropriate levels
- **Documentation**: Docstrings for all public methods and classes

## Security Considerations

### Isolation Boundaries

**Hardware Isolation**: QEMU provides complete isolation through:
- Separate virtual machines with independent kernel instances
- Hardware-assisted memory management preventing cross-VM access
- Hypervisor security boundary enforcement

**Network Isolation**: VMs operate in isolated network environments:
- Only specified ports (mcp-proxy) are accessible from host
- No direct network access between VMs
- Firewall rules enforce minimal connectivity

**Filesystem Isolation**: Workspace sharing is controlled and limited:
- Only `/workspace` directory is shared between host and guest
- No access to host filesystem outside shared directories

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

## License

MIT License - see LICENSE file for details.
