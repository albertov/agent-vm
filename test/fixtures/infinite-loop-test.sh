#!/usr/bin/env bash

# Simple infinite loop script for testing escape sequence termination

echo "Infinite loop test started"

while true; do
    printf "> "

    if ! read -r line; then
        echo "EOF"
        break
    fi

    case "$line" in
        quit)
            echo "Normal exit"
            break
            ;;
        *)
            echo "Echo: $line"
            ;;
    esac
done

echo "Loop finished"
