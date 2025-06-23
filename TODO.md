# TODO: Agent Isolation using QEMU VMs

## 🎯 Current Status: Planning QEMU VM Implementation

**Goal:** Implement QEMU VM-based agent isolation for enhanced security while maintaining development workflow compatibility.

Read README.md for specs

## Implementation Steps

### URGENT

- [ ] Address FIXME you find in agent_vm/vm_controller.py
- [ ] Fix this failing test: `integration-test --verbose --debug --timeout 90 run -- -k test_agent_service_startup`

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
