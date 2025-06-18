#!/usr/bin/env bash
set -eu -o pipefail

# Check dependencies
command -v git >/dev/null 2>&1 || { echo "Error: git not found"; exit 1; }
command -v mcp-language-server >/dev/null 2>&1 || { echo "Error: mcp-language-server not found"; exit 1; }
command -v haskell-language-server >/dev/null 2>&1 || { echo "Error: haskell-language-server not found"; exit 1; }

# Check if we're in a git repository
if ! git rev-parse --git-dir >/dev/null 2>&1; then
    echo "Error: Not in a git repository"
    exit 1
fi

REPO_ROOT="$(git rev-parse --show-toplevel)"

# Verify the workspace exists and contains Haskell files
if [[ ! -d "$REPO_ROOT" ]] || ! find "$REPO_ROOT" -name "*.hs" -o -name "*.cabal" -o -name "stack.yaml" | grep -q .; then
    echo "Warning: No Haskell project detected in $REPO_ROOT"
fi

exec mcp-language-server \
  --workspace "${REPO_ROOT}" \
  --lsp haskell-language-server -- --lsp
