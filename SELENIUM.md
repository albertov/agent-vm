# Selenium Server SystemD Jail Configuration

This document summarizes the systemd security configuration for Selenium Server with browser support on NixOS.

## Current Status

- ✅ **Firefox**: Works perfectly out of the box
- ✅ **Chrome**: Works perfectly after enabling user namespaces

## The Chrome Fix

Chrome was crashing with a misleading "user data directory in use" error. The root cause was `security.allowUserNamespaces = false` in `nix/vm-config.nix`.

**Solution**: Set `security.allowUserNamespaces = true` in the VM configuration (`nix/vm-config.nix`).

With this change, Chrome works with default settings - no special flags needed:
```javascript
{
  "headless": true
}
```

## Why It Works

Chrome's sandbox requires user namespaces to create isolated processes. When the kernel blocks namespace creation (via `security.allowUserNamespaces = false`), Chrome crashes during startup. The systemd service-level setting `RestrictNamespaces = false` isn't enough - the kernel-level restriction takes precedence.

## Configuration Overview

### Key SystemD Settings (`nix/modules/selenium-server.nix`)

1. **Virtual Display**: Service wrapped with `xvfb-run -a` for headless operation
2. **Relaxed Security**:
   - `NoNewPrivileges = false` - Chrome sandbox setup
   - `PrivateTmp = false` - Browser temp file access
   - `RestrictNamespaces = false` - Browser process isolation
   - `RemoveIPC = false` - Chrome inter-process communication
3. **File Access**:
   - ReadWrite: `/var/lib/selenium-server`, `/tmp`, `/var/tmp`, `/run/user`, `/dev/shm`
   - ReadOnly: `/nix/store`, `/etc/fonts`, system configs
4. **Environment Variables**:
   - `MOZ_FAKE_NO_SANDBOX = "1"` - Firefox compatibility
   - `CHROME_NO_SANDBOX = "1"` - Fallback for containerized environments

### VM Configuration (`nix/vm-config.nix`)

- `security.allowUserNamespaces = true` - **Critical for Chrome**
- Selenium server enabled with `services.selenium-server.enable = true`
- MCP proxy configured with selenium MCP server

## Security Considerations

The configuration relaxes several security features for browser compatibility:
- User namespaces enabled system-wide (required for Chrome sandbox)
- Reduced process isolation
- Broader filesystem access
- Disabled system call filtering

### Re-enabled Security Features
The following security features have been successfully re-enabled and tested:

**Working features:**
- `ProtectKernelTunables = true` - Browsers don't modify kernel parameters
- `ProtectKernelLogs = true` - Browsers don't need kernel logs
- `ProtectKernelModules = true` - Browsers don't load kernel modules
- `LockPersonality = true` - Browsers don't change execution domain
- `ProtectControlGroups = true` - Browsers only need read access (via bind mount)
- `ProtectSystem = "full"` - Makes /usr, /boot, /efi read-only
- `PrivateDevices = true` - Private /dev with only essential devices
- `DevicePolicy = "closed"` - Explicit device allowlist for browser needs
- `RestrictSUIDSGID = true` - Chrome uses namespace sandbox instead of setuid
- `PrivateTmp = true` - Isolated /tmp and /var/tmp directories

**Failed features:**
- `SystemCallFilter` - Chrome aborts internally (SIGABRT) when any SystemCallFilter is present
  - Even with very permissive filters, Chrome detects the seccomp filter and refuses to run
  - This is a known Chrome behavior - it's extremely sensitive to its sandboxing environment
  - Firefox also fails, likely for similar reasons
  - Recommendation: Keep SystemCallFilter disabled for browser automation

For production use, consider:
1. Running in a dedicated VM or container
2. Network isolation where possible
3. Regular security monitoring
4. Re-enabling security features that don't break functionality
