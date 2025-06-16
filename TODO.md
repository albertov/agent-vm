# TODO: Agent Isolation using QEMU VMs

## 🎯 Current Status: Planning QEMU VM Implementation

**Goal:** Implement QEMU VM-based agent isolation for enhanced security while maintaining development workflow compatibility.

Read README.md for specs

# TODO: Agent Isolation using QEMU VMs

## 🎯 Current Status: Planning QEMU VM Implementation

**Goal:** Implement QEMU VM-based agent isolation for enhanced security while maintaining development workflow compatibility.

Read README.md for specs

## Implementation Steps

### URGENT

- [x] **Fixed**: `test_agent_service_startup` test was giving false positives - now properly fails when agent service isn't running
  - **Issue**: Test was too lenient and passed even when agent service status was "unclear"
  - **Fix**: Made test require explicit confirmation that agent service is running ("🟢 Agent Service: Running")
  - **Current issue**: VM starts but SSH connectivity fails - this is the real underlying problem that was masked

- [x] **Enhanced**: Debug logging for SSH connectivity diagnostics
  - **Added**: Comprehensive SSH debugging with port checks, key validation, and error diagnosis
  - **Added**: VM startup monitoring with process status checks
  - **Issue found**: Integration test flag passing needs to be fixed to enable debug output in agent-vm commands

- [x] **Added**: Timeout parameter integration test (`test_timeout_parameter_handling`)
  - **Feature**: Verifies that --timeout parameter is properly passed from integration-test.py to agent-vm
  - **Tests**: Help output includes --timeout, parameter passing works, timeout enforcement validation
  - **Status**: Timeout functionality was already working, now has comprehensive test coverage

### DEBUG LOGGING STATUS

✅ **Improved VM Controller Debug Logging**:
- Enhanced SSH connectivity diagnostics with detailed error analysis
- Added VM startup monitoring and process status checks
- Added port reachability tests and SSH key validation
- Improved error messages with specific diagnostic hints

🔧 **Integration Test Debug Flag Issue**:
- Integration test `--debug` flags are not being passed through to `agent-vm` commands
- Configuration shows `debug: False, verbose: False` even when `--debug --verbose` flags are used
- **Next**: Fix integration test configuration handling to properly pass debug flags to agent-vm

### KNOWN ISSUES

- [ ] **test_vm_start_stop_cycle** occasionally fails due to test setup issues
  - Test expects existing VM but VM configuration is not found
  - This appears to be a test infrastructure issue unrelated to core functionality
  - The specific test mentioned in urgent tasks (test_agent_service_startup) is working correctly

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
  - Network traffic monitoring with mitm-proxy and a self signe cert which is
    installed into the VM cert store

- [ ] **Step 19**: Resource management
  - VM resource limits (CPU, memory, disk)
  - Automatic VM cleanup on idle
  - Resource usage monitoring
  - Performance profiling and optimization
