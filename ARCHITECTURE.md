# Agent VM Architecture

## Overview

The Agent VM project implements secure, hardware-isolated execution environments for MCP (Model Context Protocol) agents using **NixOS QEMU virtual machines**. This architecture provides complete isolation while maintaining seamless development workflows and preserving all existing MCP functionality.

**Key Design Principles:**
- **Security First**: Hardware-level isolation via QEMU VMs
- **Developer Experience**: Seamless integration with existing workflows
- **Minimal Dependencies**: Python stdlib-only implementation for maximum portability
- **Backward Compatibility**: Preserve existing MCP server functionality and interfaces

## System Architecture

### High-Level Component Overview

```
┌─────────────────────────────────────────────────────────────┐
│                        Host System                          │
│  ┌─────────────────┐  ┌──────────────────────────────────┐  │
│  │   agent-vm      │  │        VM Instance               │  │
│  │  (Python CLI)   │  │  ┌─────────────────────────────┐  │  │
│  │                 │◄─┤  │      NixOS Guest            │  │  │
│  │ • VM Lifecycle  │  │  │  ┌─────────────────────────┐│  │  │
│  │ • SSH Access    │  │  │  │   agent-mcp.service     ││  │  │
│  │ • Status Mgmt   │  │  │  │  (systemd service)      ││  │  │
│  └─────────────────┘  │  │  │                         ││  │  │
│           │            │  │  │ • mcp-proxy             ││  │  │
│           │            │  │  │ • codemcp               ││  │  │
│           │            │  │  │ • mcp-language-server   ││  │  │
│  ┌─────────────────┐  │  │  │ • selenium              ││  │  │
│  │  Workspace      │  │  │  │ • rescript-lsp          ││  │  │
│  │  (VirtioFS)     │◄─┼──┼──┤ • mcp-nixos             ││  │  │
│  │                 │  │  │  └─────────────────────────┘│  │  │
│  │ • Git Repo      │  │  │                             │  │  │
│  │ • SSH Keys      │  │  │  Port Forwarding:           │  │  │
│  │ • Config Files  │  │  │  • 2222 → 22 (SSH)         │  │  │
│  └─────────────────┘  │  │  • 8000 → 8000 (MCP)       │  │  │
│                        │  └─────────────────────────────┘  │  │
└─────────────────────────────────────────────────────────────┘
```

### Core Components

#### 1. VM Management Layer (`agent-vm.py`)

**Purpose**: Complete VM lifecycle management via Python CLI application

**Key Features:**
- **Zero Dependencies**: Python stdlib only for maximum portability
- **Branch Isolation**: One VM configuration per git branch
- **SSH Key Management**: Automatic generation and secure storage
- **Workspace Synchronization**: Git repository cloning and remote setup
- **Process Tracking**: PID-based process management with graceful shutdown

**Command Interface:**
```bash
agent-vm create [--branch=<name>] [--port=<port>]  # Create VM config
agent-vm start [<branch>]                          # Start VM + services
agent-vm stop [<branch>]                           # Graceful shutdown
agent-vm status [<branch>]                         # Show VM/service status
agent-vm shell [<branch>]                          # SSH into VM
agent-vm logs [<branch>]                           # View service logs
agent-vm list                                      # List all VMs
agent-vm destroy [<branch>]                        # Remove VM config
```

#### 2. VM Configuration Layer (`vm-config.nix`)

**Purpose**: NixOS configuration for secure, isolated VM instances

**Key Specifications:**
- **Resources**: 4GB RAM, 4 CPU cores, 20GB disk
- **Isolation**: Headless mode, network isolation, minimal attack surface
- **Sharing**: VirtioFS high-performance workspace sharing
- **Access**: SSH on port 2222, MCP proxy on port 8000
- **Security**: Firewall rules, disabled user namespaces, kernel protection

**Architecture Integration:**
```nix
virtualisation.vmVariant = {
  virtualisation = {
    memorySize = 4096; cores = 4; diskSize = 20480;
    graphics = false;  # Headless for performance
    sharedDirectories.workspace = {
      source = "/workspace"; target = "/workspace";
      securityModel = "mapped-xattr";  # Secure file permissions
    };
    forwardPorts = [
      { from = "host"; host.port = 8000; guest.port = 8000; }  # MCP
      { from = "host"; host.port = 2222; guest.port = 22; }    # SSH
    ];
  };
};
```

#### 3. Agent Service Layer (`agent-service.nix`)

**Purpose**: Systemd service management for MCP agents within VMs

**Security Model:**
- **Minimal Privileges**: NoNewPrivileges, ProtectSystem=strict
- **Restricted Access**: ReadWritePaths limited to workspace only
- **Process Isolation**: PrivateTmp, PrivateDevices, ProtectKernelModules
- **Network Control**: Controlled network access for MCP proxy only
- **Service Recovery**: Automatic restart with exponential backoff

**Service Configuration:**
```nix
systemd.services.agent-mcp = {
  serviceConfig = {
    Type = "exec";
    User = cfg.user; Group = cfg.group;
    WorkingDirectory = cfg.workspaceDir;

    # Security hardening
    NoNewPrivileges = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    ReadWritePaths = [ cfg.workspaceDir ];

    # Health checking
    Restart = "always";
    RestartSec = "5s";
    ExecStartPost = "health-check-script";
  };
};
```

### Data Flow Architecture

#### VM Creation Flow
```
user: agent-vm create
  ↓
1. Create VM config directory (~/.local/share/agent-vms/<branch>/)
2. Generate SSH keypair (id_ed25519, id_ed25519.pub)
3. Clone git repository to workspace/
4. Set up git remotes (origin, upstream)
5. Store VM metadata (config.json)
  ↓
VM Ready for Start
```

#### VM Startup Flow
```
user: agent-vm start
  ↓
1. Load VM configuration from config.json
2. Build NixOS VM with injected SSH public key
3. Start QEMU process with VM naming and PID tracking
4. Wait for SSH connectivity (retry logic)
5. Start agent-mcp.service via systemctl
6. Verify MCP proxy health endpoint
  ↓
Agent Ready (http://localhost:8000)
```

#### Development Workflow
```
user: agent-vm shell
  ↓
SSH into VM → /workspace (shared via VirtioFS)
  ↓
Edit files → Changes visible on host instantly
  ↓
MCP tools available → Same environment as direct execution
  ↓
Git operations → Sync with host repository
```

## Storage Architecture

### Host Storage Layout
```
~/.local/share/agent-vms/
├── <branch-name-1>/
│   ├── config.json          # VM metadata
│   ├── vm.pid               # QEMU process ID
│   ├── ssh/
│   │   ├── id_ed25519       # Private key (600)
│   │   └── id_ed25519.pub   # Public key (644)
│   └── workspace/           # Git repository clone
│       ├── .git/
│       ├── agent-vm.py
│       ├── vm-config.nix
│       └── [project files]
├── <branch-name-2>/
│   └── [same structure]
```

### VM Storage Layout
```
/workspace/                  # VirtioFS mount from host
├── [same as host workspace/]
/home/dev/                   # VM user home
├── [standard user files]
/etc/nixos/                  # VM system config
├── configuration.nix → vm-config.nix
```

### Security Boundaries

#### Hardware Isolation
- **QEMU/KVM**: Hardware-assisted virtualization boundary
- **Memory Protection**: MMU-enforced memory isolation between VMs
- **Network Isolation**: Separate network namespaces per VM
- **Filesystem Isolation**: Separate root filesystems with controlled sharing

#### Software Isolation
- **systemd Hardening**: Comprehensive service restrictions
- **User Privileges**: Non-root execution with minimal capabilities
- **Network Policies**: Firewall rules limiting exposed ports
- **Kernel Protection**: Locked modules, protected kernel image

## Integration Architecture

### MCP Tool Integration

The agent service integrates with the existing MCP ecosystem by leveraging `mkMCPDevServers`:

```nix
config = lib.mkIf cfg.enable {
  systemd.services.agent-mcp = let
    devServers = pkgs.mkMCPDevServers {
      inherit (cfg) name shell pkgs;
    };
  in {
    # Service runs the same MCP servers as direct execution
    ExecStart = "${devServers}/bin/start-agent";
  };
};
```

**Available MCP Servers:**
- **codemcp**: Core MCP functionality for code operations
- **mcp-proxy**: HTTP proxy for web-based MCP clients
- **mcp-language-server**: Language server integrations
- **rescript-lsp**: ReScript language server via mcp_rescript_lsp.sh
- **mcp-nixos**: NixOS configuration management tools
- **selenium**: Web automation via mcp_selenium.sh

### Development Environment Consistency

The VM environment includes the same packages and environment as the host development shell:

```nix
users.users.dev = {
  # Same packages available as in mkMCPDevServers shell
  packages = config.services.agent-mcp.shell.buildInputs;
};
```

This ensures that development workflows remain identical whether running directly or in a VM.

## Performance Characteristics

### VM Startup Performance
- **Cold Start**: ~15-30 seconds (includes NixOS boot)
- **SSH Ready**: ~10-20 seconds after QEMU start
- **Service Ready**: ~5-10 seconds after SSH connectivity
- **Total Time**: ~30-60 seconds from `agent-vm start` to ready

### Runtime Performance
- **VirtioFS Overhead**: <5% for typical file operations
- **Network Overhead**: Minimal for HTTP-based MCP communication
- **Memory Overhead**: ~512MB for VM OS + configured memory allocation
- **CPU Overhead**: <10% for compute-intensive operations

### Optimization Strategies
- **Memory Backend**: Shared memory via memfd for VirtioFS performance
- **NUMA Configuration**: Optimal memory placement for shared directories
- **Headless Mode**: No graphics reduces resource usage
- **Minimal OS**: NixOS configuration includes only essential services

## Operational Considerations

### Monitoring and Health Checks
- **VM Process Monitoring**: PID-based process tracking
- **SSH Connectivity**: Automated connectivity verification
- **Service Health**: MCP proxy health endpoint checking
- **Resource Usage**: Monitor VM memory and CPU consumption

### Error Recovery
- **VM Startup Failures**: Automatic cleanup of failed VM processes
- **SSH Connection Issues**: Retry logic with exponential backoff
- **Service Failures**: systemd automatic restart with limits
- **Process Cleanup**: Graceful shutdown with SIGTERM/SIGKILL fallback

### Multi-Instance Support
- **Branch Isolation**: Each git branch gets separate VM configuration
- **Port Management**: Automatic port allocation for concurrent VMs
- **Resource Limits**: Per-VM resource constraints via QEMU options
- **Workspace Isolation**: Separate workspaces prevent cross-contamination

## Migration and Compatibility

### Backward Compatibility
- **mkMCPDevServers**: Existing function preserved with VM support added
- **Shell Hooks**: Development environment setup maintained
- **MCP Interfaces**: All MCP servers function identically in VM
- **Command Interface**: `nix run .#agent` works with both modes

### Migration Strategy
- **Gradual Adoption**: VM mode opt-in initially, direct execution fallback
- **Feature Parity**: Ensure VM mode matches direct execution functionality
- **Performance Validation**: Monitor and optimize VM performance characteristics
- **User Training**: Documentation and migration guides for developers

### Future Extensibility
- **Multiple VM Types**: Support for different VM configurations
- **Cloud Integration**: Potential for remote VM deployment
- **Resource Scaling**: Dynamic resource allocation based on workload
- **Advanced Networking**: VPN integration, custom network topologies

## Security Model

### Threat Model
**Mitigated Threats:**
- **Code Injection**: VM boundary prevents host system compromise
- **Privilege Escalation**: systemd hardening limits service capabilities
- **Data Exfiltration**: Network isolation controls outbound connections
- **Resource Exhaustion**: VM resource limits prevent DoS attacks

**Attack Surface Reduction:**
- **Minimal VM OS**: Only essential services enabled
- **Restricted Network Access**: Only SSH and MCP proxy ports exposed
- **No User Namespaces**: Prevents container escape techniques
- **Locked Kernel**: Module loading and kernel image modification prevented

### Security Monitoring
- **Process Auditing**: Track VM process lifecycle
- **Network Monitoring**: Log connection attempts and data transfer
- **Resource Monitoring**: Detect resource exhaustion attempts
- **Access Logging**: SSH and service access logging

This architecture provides a robust foundation for secure agent execution while maintaining developer productivity and system reliability.
