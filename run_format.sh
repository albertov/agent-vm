#!/usr/bin/env bash
set -e -o pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"

RROF_ROOT="$REPO_ROOT/re-react-onping-frontend/"

format_file() {
    local file="$1"
    
    case "$file" in
        *.hs)
            echo "Formatting Haskell file: $file"
            fourmolu -i "$file"
            ;;
        *.res|*.resi)
            echo "Formatting ReScript file: $file"
            "$REPO_ROOT/agents/rescript_sys.sh" format "$file"
            ;;
        *.nix)
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
