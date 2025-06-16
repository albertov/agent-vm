# TODO: Agent Isolation using QEMU VMs

## 🎯 Current Status: Planning QEMU VM Implementation

**Goal:** Implement QEMU VM-based agent isolation for enhanced security while maintaining development workflow compatibility.

Read README.md for specs

## Implementation Steps

### URGENT

- [x] Refactor to use typer for CLI.
- [x] Make agent service run properly inside the VM
- [x] We should probe for a free port starting at 2222 to forward ssh, not hardcode 2222

### Phase 4: Security and Performance

- [ ] **Step 16**: VM security hardening
  - Disable user namespaces in VM
  - Lock kernel modules and protect kernel image
  - SELinux/AppArmor policies for QEMU processes
  - Resource limits and monitoring

- [ ] **Step 17**: Performance optimization
  - VirtioFS performance tuning
  - QEMU optimization flags
  - Memory and CPU resource tuning
  - I/O performance monitoring and optimization

- [ ] **Step 18**: Network security
  - Minimal firewall rules (SSH + MCP only)
  - Network namespace isolation
  - TLS/encryption for MCP communication
  - Network traffic monitoring

- [ ] **Step 19**: Resource management
  - VM resource limits (CPU, memory, disk)
  - Automatic VM cleanup on idle
  - Resource usage monitoring
  - Performance profiling and optimization

### Phase 5: Documentation and Testing

- [x] **Step 20**: Documentation updates
  - ✅ Consolidated all markdown documentation into comprehensive README.md
  - ✅ Added detailed project structure overview with file descriptions
  - ✅ Complete usage examples and workflows
  - ✅ Comprehensive troubleshooting guide
  - ✅ Security considerations and threat model
  - ✅ Performance characteristics and optimization guidance
  - ✅ Removed redundant documentation files

- [x] **Step 21**: Testing and validation
  - ✅ Add pytest integration test suite
  - ✅ VM creation and startup testing
  - ✅ MCP server functionality verification
  - ✅ Multi-instance testing
  - ✅ Error condition testing
  - ✅ Convert agent-vm to proper Python package with pyproject.toml

- [ ] **Step 22**: Migration testing
  - Side-by-side comparison with direct execution
  - Performance benchmarking
  - Security validation
  - User acceptance testing

- [ ] **Step 23**: Production deployment
  - Default switch to VM mode
  - Migration guide for users
  - Monitoring and alerting setup
  - Rollback procedures if needed

## Technical Implementation Details

### VM Configuration Requirements

```nix
# Key configuration elements needed
{
  # Import agent service module
  imports = [ ./agent-service.nix ];

  # Enable agent service
  services.agent-mcp = {
    enable = true;
    user = "dev";
    group = "dev";
    workspaceDir = "/workspace";
    port = 8000;
    allowedOrigin = "https://claude.ai";
  };

  virtualisation.vmVariant = {
    virtualisation = {
      memorySize = 4096;
      cores = 4;
      diskSize = 20480;
      graphics = false;
      sharedDirectories.workspace = {
        source = "/workspace";
        target = "/workspace";
        securityModel = "mapped-xattr";
      };
      forwardPorts = [
        { from = "host"; host.port = 8000; guest.port = 8000; }
        { from = "host"; host.port = 2222; guest.port = 22; }
      ];
    };
  };

  # Include all MCP tools (same as mkMCPDevServers via agent service)
  # No longer needed in environment.systemPackages - handled by agent service
  # User packages are inherited from config.services.agent-mcp.shell.buildInputs
}
```

### Agent Service Module Requirements

```nix
# Key NixOS module elements needed for agent-service.nix
{
  options.services.agent-mcp = {
    enable = lib.mkEnableOption "Agent MCP service";
    user = lib.mkOption { type = lib.types.str; default = "agent"; };
    workspaceDir = lib.mkOption { type = lib.types.path; default = "/workspace"; };
    port = lib.mkOption { type = lib.types.port; default = 8000; };
    allowedOrigin = lib.mkOption { type = lib.types.str; default = "https://claude.ai"; };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.agent-mcp = {
      description = "Agent MCP Service";
      serviceConfig = {
        # Security hardening with minimal capabilities
        NoNewPrivileges = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ReadWritePaths = [ cfg.workspaceDir ];
        User = cfg.user;
        Group = cfg.group;
        # Health checking and restart policies
        Restart = "always";
        RestartSec = "5s";
      };
    };
  };
}
```

### Python Implementation Requirements

```python
# Key methods needed in agent-vm Python application
class VMController:
    def _is_vm_running()      # Check if VM is active
    def _wait_for_vm_ready()  # Wait for SSH accessibility
    def start_vm()            # Build and start VM
    def stop_vm()             # Graceful shutdown
    def _start_agent_in_vm()  # Start agent via systemctl (systemctl start agent-mcp)
    def _wait_for_agent_ready() # Wait for agent service health check
    def vm_status()           # Display VM/agent status using systemctl
    def vm_shell()            # Open interactive shell
    def vm_logs()             # Show agent service logs via journalctl
    def create_vm()           # Create VM configuration
    def destroy_vm()          # Destroy VM configuration
    def list_vms()            # List all VM configurations
```

### Integration Points

- **mkMCPDevServers**: Add VM mode support with `useVM` parameter
- **Flake apps**: Create VM and direct execution variants
- **Runtime inputs**: Add QEMU, SSH, and VM management tools
- **Shell hooks**: Preserve existing development environment setup
- **MCP Tools**: Same tools available in VM as in direct execution

## Success Criteria

- [ ] **Functional**: VM starts and runs MCP servers successfully
- [ ] **Compatible**: Same interface and behavior as direct execution
- [ ] **Secure**: Enhanced isolation with hardware virtualization
- [ ] **Performant**: Acceptable overhead for development workflows
- [ ] **Reliable**: Stable operation with proper error handling
- [ ] **Maintainable**: Clean code with good documentation

## Risk Mitigation

**Performance Concerns:**
- Monitor VM startup time and resource usage
- Optimize VirtioFS and QEMU configuration
- Provide direct execution fallback for resource-constrained environments

**Compatibility Issues:**
- Maintain same interface and user experience
- Ensure all MCP servers work identically in VM
- Preserve existing development workflow patterns

**Complexity Overhead:**
- Keep VM management scripts simple and robust
- Provide clear documentation and troubleshooting guides
- Implement comprehensive error handling and recovery

## Critical Issues to Fix

- [x] **Issue 1**: Add configurable timeout to integration test (default 60s)
  - ✅ Updated integration-test.py to accept --timeout parameter
  - ✅ Changed VM start test to use configurable timeout instead of hardcoded 300s
  - ✅ Updated help text and argument parsing

- [x] **Issue 2**: Fix agent service startup failures in VM
  - ✅ **Root cause**: Several configuration and script issues preventing agent-mcp service from starting
  - ✅ **Fixed Sub-issues**:
    - ✅ Fixed start.sh variable assignment bugs (lines 5-6 used wrong variable names)
    - ✅ Added missing MCP tools to agent service shell environment
    - ✅ Fixed user/group configuration mismatch between vm-config.nix and agent-service.nix
    - ✅ Removed circular dependency in shell configuration in agent-service.nix
    - ✅ Completed MCP server configuration in start.sh (enabled rescript-lsp)
    - ✅ Fixed user group creation logic to avoid conflicts with existing dev user

- [x] **Issue 3**: Fix SSH connectivity and sudo issues in integration tests
  - ✅ **Root cause**: Integration tests fail because:
    - SSH connectivity check lacks detailed logging
    - Agent service startup uses sudo without passwordless configuration
    - Service should auto-start but manual start attempted anyway
  - ✅ **Solution**: Added comprehensive debug logging with --debug flag
    - Enhanced integration test with --debug flag for full stderr/stdout capture
    - Process output is now logged on timeouts, failures, and when debug mode is enabled
    - Added detailed timeout diagnostics and troubleshooting hints
    - Debug mode captures all subprocess output for easier debugging

## Current Implementation Status

**✅ COMPLETED:**
- Research and specification complete
- Architecture design finalized
- Implementation plan documented
- **README.md**: Comprehensive architecture documentation created
- VM configuration specification updated with:
  - Dynamic firewall configuration using service port
  - User package inheritance from agent service
  - AllowedOrigin configuration for MCP proxy
  - Removal of redundant environment.systemPackages
  - Updated system state version to 24.11
- **Phase 0: Package Setup**
  - ✅ Step 0: Added `agent-vm` Python application package derivation to overlay.nix
- **Phase 1: Core VM Infrastructure**
  - ✅ Steps 1-4: VM configuration, workspace sharing, networking, and lifecycle management implemented
- **Phase 2: Development Integration**
  - ✅ Steps 4.5-10: Complete flake integration, VM management tool, and agent service implementation
- **Phase 3: Robust VM Management**
  - ✅ Steps 11-15: SSH authentication, status monitoring, package structure, UX improvements, and error handling

### Phase 6: Integration Testing and CLI Enhancements

- [x] **Step 24**: Add CLI argument for state directory override
  - ✅ Add `--state-dir` global argument to agent-vm CLI
  - ✅ Modify VMController to accept optional base_dir parameter
  - ✅ Update all VM commands to use custom state directory when specified
  - ✅ Ensure backward compatibility with default `~/.local/share/agent-vms`

- [x] **Step 25**: Create integration test executable
  - ✅ Create standalone integration test executable (not part of normal test suite)
  - ✅ Test executable should call agent_py through CLI exclusively (no mocks)
  - ✅ Use custom state directory for test isolation
  - ✅ Include comprehensive workflow testing (create, start, test, stop, destroy)
  - ✅ Make executable available as flake app for easy running

- [x] **Step 26**: Improve integration tests with port probing and cleanup
  - ✅ Add port probing functionality to find free ports starting at 12000
  - ✅ Ensure temporary directories are ALWAYS deleted (even in case of test failure)
  - ✅ Update integration test script to use dynamic port allocation

- [x] **Step 27**: Add subprocess output capture with tempfile logging
  - ✅ Capture stdout and stderr of every subprocess to tempfiles
  - ✅ Destroy tempfiles at program exit (regardless of exceptions)
  - ✅ Display last 20 lines and paths as log.error on subprocess failures

**🔄 IN PROGRESS:**
- **Step 16**: VM security hardening
  - Disable user namespaces in VM
  - Lock kernel modules and protect kernel image
  - SELinux/AppArmor policies for QEMU processes
  - Resource limits and monitoring
- Multi-instance support (Step 16 alternate)
- Phase 4-5 implementation phases (security, performance, documentation)
- Testing and validation
- Integration testing

## Notes

- **Priority**: Security-focused implementation with acceptable performance overhead
- **Approach**: Gradual migration with fallback options
- **Goal**: Enhanced security without compromising development experience
- **Timeline**: Implement incrementally with continuous testing and validation
- **Environment Setup**: VM includes same MCP tools as mkMCPDevServers, no need for `nix develop`
- **agent-vm**: Built as typed Python application using `buildPythonApplication` with stdlib only
- **Recent Updates**: Specification refined with dynamic configuration, proper package inheritance, and security improvements (June 2025)
