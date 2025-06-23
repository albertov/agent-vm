#!/usr/bin/env bash
set -e -o pipefail

format_file() {
    local file="$1"
    
    case "$file" in
        *.nix|*.py|*.sh|*.hs)
            echo "Formatting file with nix fmt: $file"
            nix fmt "$file"
            ;;
        *)
            echo "Unknown file: $file"
            ;;
    esac
}

for file in "$@"; do
    format_file "$file"
done
