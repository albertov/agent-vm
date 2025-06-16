#!/usr/bin/env bash
set -eu -o pipefail

exec nix run github:albertov/mcp-selenium-haskell
