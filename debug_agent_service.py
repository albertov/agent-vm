#!/usr/bin/env python3
"""Debug script to check agent-mcp service availability in VM"""

import subprocess
import sys
import json
from pathlib import Path


def run_cmd(cmd):
    """Run command and return result"""
    print(f"Running: {' '.join(cmd)}")
    try:
        result = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
        print(f"Exit code: {result.returncode}")
        print(f"Stdout: {result.stdout}")
        print(f"Stderr: {result.stderr}")
        return result
    except subprocess.TimeoutExpired:
        print("Command timed out")
        return None
    except Exception as e:
        print(f"Error: {e}")
        return None


def main():
    # First, let's check if there are any existing VMs
    print("=== Checking existing VMs ===")
    result = run_cmd(["nix", "run", ".#agent-vm", "--", "list"])

    # Find a test VM
    test_branch = None
    if result and result.returncode == 0:
        lines = result.stdout.strip().split('\n')
        for line in lines:
            if "integration-test" in line and "Running" in line:
                # Extract branch name
                parts = line.split()
                if len(parts) > 0:
                    test_branch = parts[0].strip()
                    break

    if not test_branch:
        print("\nNo running test VM found. Creating one...")
        # Create a test VM
        test_branch = "debug-agent-test"
        run_cmd(["nix", "run", ".#agent-vm", "--", "create", "--branch", test_branch, "--port", "9999"])
        print("\nStarting the VM...")
        run_cmd(["nix", "run", ".#agent-vm", "--", "start", test_branch])
    else:
        print(f"\nUsing existing test VM: {test_branch}")

    # Get VM config
    state_dir = Path.home() / ".local/share/agent-vms" / test_branch
    config_file = state_dir / "config.json"

    if not config_file.exists():
        print(f"Error: Config file not found at {config_file}")
        sys.exit(1)

    with open(config_file) as f:
        config = json.load(f)

    ssh_key = config["ssh_key_path"]
    ssh_port = config.get("ssh_port", 2222)

    print(f"\n=== VM Configuration ===")
    print(f"Branch: {test_branch}")
    print(f"SSH Port: {ssh_port}")
    print(f"SSH Key: {ssh_key}")

    # Now let's check the systemd services in the VM
    print("\n=== Checking systemd services in VM ===")

    # Check if agent-mcp.service exists
    ssh_cmd = [
        "ssh", "-o", "ConnectTimeout=10", "-o", "StrictHostKeyChecking=no",
        "-o", "UserKnownHostsFile=/dev/null", "-i", ssh_key,
        "-p", str(ssh_port), "dev@localhost"
    ]

    print("\n1. Listing all systemd services:")
    run_cmd(ssh_cmd + ["systemctl", "list-units", "--type=service", "--all"])

    print("\n2. Checking agent-mcp unit file:")
    run_cmd(ssh_cmd + ["systemctl", "list-unit-files", "agent-mcp.service"])

    print("\n3. Checking agent-mcp service status:")
    run_cmd(ssh_cmd + ["systemctl", "status", "agent-mcp.service"])

    print("\n4. Checking journalctl for agent-mcp:")
    run_cmd(ssh_cmd + ["sudo", "journalctl", "-u", "agent-mcp", "-n", "50"])

    print("\n5. Checking if start-agent exists:")
    run_cmd(ssh_cmd + ["which", "start-agent"])

    print("\n6. Checking PATH:")
    run_cmd(ssh_cmd + ["echo", "$PATH"])

    print("\n7. Checking nix profile:")
    run_cmd(ssh_cmd + ["ls", "-la", "/run/current-system/sw/bin/"])

    # Clean up if we created a VM
    if test_branch == "debug-agent-test":
        print(f"\n=== Cleaning up test VM ===")
        run_cmd(["nix", "run", ".#agent-vm", "--", "stop", test_branch])
        run_cmd(["nix", "run", ".#agent-vm", "--", "destroy", test_branch])


if __name__ == "__main__":
    main()
