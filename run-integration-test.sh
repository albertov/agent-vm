#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")/agent-vm"
exec ./dist-newstyle/build/x86_64-linux/ghc-9.10.2/agent-vm-0.1.0.0/x/agent-vm-test/noopt/build/agent-vm-test/agent-vm-test "$@"
