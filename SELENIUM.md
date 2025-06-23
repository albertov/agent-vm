# Selenium Server SystemD Jail Configuration

This document summarizes the changes needed to make Selenium work with browsers in a systemd-sandboxed environment on NixOS.

## Problem Summary

The default systemd security hardening prevents browsers (Chrome/Firefox) from functioning properly with Selenium due to:
- Namespace restrictions preventing browser sandboxing
- Missing access to shared memory and temporary directories
- System call filtering blocking critical browser operations
- Missing display environment for rendering
- Device access restrictions

## Working Configuration

### Key Changes Made

1. **Virtual Display (xvfb-run)**
   - Wrapped selenium-server with `xvfb-run -a` to provide virtual framebuffer
   - Eliminates "no DISPLAY environment variable" errors
   - Automatically manages display allocation and cleanup

2. **Disabled Security Features**
   - `NoNewPrivileges = false` - Chrome needs to set up its sandbox
   - `PrivateTmp = false` - Browsers need access to real /tmp directories
   - `PrivateDevices = false` - Chrome needs various device access
   - `RestrictNamespaces = false` - Browsers create sandboxed child processes
   - `RemoveIPC = false` - Chrome uses IPC between processes
   - `LockPersonality = false` - Chrome might need to change execution domain
   - `RestrictSUIDSGID = false` - Chrome sandbox helper might be setuid
   - `ProtectSystem = false` - Chrome needs broad filesystem access
   - `SystemCallFilter` disabled - Chrome requires many syscalls (ptrace, pkey_alloc, etc.)

3. **Process Visibility**
   - `ProcSubset = "all"` - Browsers need to see other processes
   - `ProtectProc = "default"` - Normal /proc visibility

4. **Filesystem Access**
   - **ReadWritePaths**:
     - `/var/lib/selenium-server` - Service home directory
     - `/tmp` - Browser downloads and cache
     - `/var/tmp` - Additional temp space
     - `/run/user` - Runtime directories
     - `/dev/shm` - Shared memory for Chrome rendering

   - **ReadOnlyPaths**:
     - `/nix/store` - All packages and dependencies
     - `/run/current-system/sw` - System-wide software
     - `/etc/fonts` - Font configuration
     - `/etc/machine-id` - Machine ID (Chrome checks this)
     - Various system config files

5. **Environment Variables**
   - `HOME = "/var/lib/selenium-server"`
   - `MOZ_FAKE_NO_SANDBOX = "1"` - Tell Firefox to skip its sandbox in containers
   - `CHROME_NO_SANDBOX = "1"` - Disable Chrome sandbox in containers

6. **Additional Configuration**
   - Added `geckodriver` to packages list
   - Added bind mount for `/sys/fs/cgroup` for browser sandboxing
   - Disabled most kernel protection features temporarily

## Current Status

- ✅ **Firefox**: Works perfectly with the configuration
- ❌ **Chrome**: Still crashes with "user data directory in use" error (misleading - actual issue is Chrome crashing during startup)

## Chrome-Specific Issues

Chrome continues to crash even with all security features disabled. The "user data directory in use" error appears to be a red herring - Chrome is actually crashing before it can properly set up its profile. This happens even with:
- All systemd security features disabled
- Unique user data directories specified
- All sandbox-disabling flags applied
- xvfb-run providing a virtual display
- All necessary paths made accessible

### Root Cause

The issue appears to be deeper than systemd configuration. Possible causes:
1. **NixOS Chrome packaging issue** - Chrome might have specific dependencies or configurations in NixOS that conflict with running under the selenium-server user
2. **Missing libraries** - Chrome might need libraries that aren't available in the service environment
3. **Chrome's internal security** - Chrome has its own security model that might conflict with running as a non-standard user
4. **ChromeDriver compatibility** - Mismatch between Chrome and ChromeDriver versions

### Potential Solutions to Try

1. **Check Chrome logs**:
   ```bash
   sudo journalctl -u selenium-server -f | grep -E "(chrome|chromium|SIGSEGV|SIGABRT)"
   ```

2. **Chrome flags to test**:
   ```javascript
   {
     "headless": true,
     "arguments": [
       "--no-sandbox",
       "--disable-setuid-sandbox",
       "--disable-dev-shm-usage",
       "--disable-gpu",
       "--disable-web-security",
       "--disable-features=VizDisplayCompositor",
       "--single-process",
       "--no-zygote",
       "--user-data-dir=/tmp/chrome-profile-unique"
     ]
   }
   ```

3. **Additional environment variables**:
   - `CHROME_DEVEL_SANDBOX` - Path to Chrome sandbox binary
   - `GOOGLE_CHROME_BIN` - Path to Chrome binary

4. **Missing dependencies** - Chrome might need additional libraries not available in the jail

## Security Considerations

The current configuration significantly relaxes security to make browsers work. For production use:

1. Re-enable security features one by one after browsers work
2. Use a dedicated user/group for selenium-server
3. Limit network access if possible
4. Monitor logs for security issues
5. Consider running in a VM or container for additional isolation

## File Location

The selenium-server systemd configuration is at: `/nix/modules/selenium-server.nix`
