# Agent VM - Secure MCP Agent Isolation System

Agent VM is a comprehensive system for running MCP (Model Context Protocol) agents in secure, isolated QEMU virtual machines. It provides enhanced security through hardware virtualization while maintaining full development workflow compatibility.

All VMs state is located in an user configurable `--state-dir` directory
(defaults to `~/.local/share/agent-vms/<repo>/<branch>`)

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

The `agent-vm create` command:

1. **Configuration Setup**: Creates VM configuration directory in `~/.local/share/agent-vms/<repo>/<branch>/`
3. **Workspace Clone**: Clones current repository branch to VM workspace using
   git worktrees
5. **Metadata Storage**: Saves VM configuration as a nix expression

### VM Startup

The `agent-vm start` command:

1. **Configuration Validation**: Verifies VM configuration exists and is valid
2. **VM Building**: Uses Nix to build VM with injected SSH keys and workspace paths
3. **Process Launch**: Starts QEMU VM process with proper naming and resource allocation

### Status Monitoring

The `agent-vm status` command provides comprehensive monitoring:

**VM Process Status**:
- Process existence and PID tracking
- Resource usage (CPU, memory, uptime)
- QEMU process health monitoring

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

### Agent Service (`services/mcp-proxy.nix`)

### State Directory Structure

VM configurations are stored in `~/.local/share/agent-vms/<repo>/<branch>/`:

```
~/.local/share/agent-vms/my-repo/
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
