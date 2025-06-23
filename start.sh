#!/usr/bin/env bash
set -eu -o pipefail
REPO_ROOT="$(git rev-parse --show-toplevel)"
MCP_PROXY_PORT="${MCP_PROXY_PORT:-8000}"
MCP_PROXY_HOST="${MCP_PROXY_HOST:-localhost}"
ALLOW_ORIGIN="${ALLOW_ORIGIN:-https://claude.ai}"
trap "git config --global --unset core.hooksPath" EXIT
# Disable pre-commit hooks because they format the source whenever the agent
# commits stuff and confuses it. Me no like getting throttled because of
# confused robots.
git config --global core.hooksPath /dev/null
# Always bind to an interface that is NOT, NOT, NOT exposed to the internet.
cd "$REPO_ROOT"
mcp-proxy \
  --port "${MCP_PROXY_PORT}" \
  --host "${MCP_PROXY_HOST}" \
  --allow-origin="${ALLOW_ORIGIN}" \
  --named-server codemcp \
    codemcp \
  --named-server selenium \
    "${REPO_ROOT}/mcp_selenium.sh" \
  --named-server rescript-lsp \
    "${REPO_ROOT}/mcp_rescript_lsp.sh"
