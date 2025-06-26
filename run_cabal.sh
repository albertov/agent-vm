#!/usr/bin/env bash
set -eu -o pipefail
exec cabal "$@" 2>&1
