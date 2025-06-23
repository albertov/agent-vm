#!/usr/bin/env bash
set -eu -o pipefail

git log -p \
  "$(git merge-base HEAD origin/main)...HEAD" -- \
  ':(exclude)golden/**' \
  ':(exclude)*.json' \
  "$@"
