#!/usr/bin/env bash

# Simple escape sequence test script
# Echoes back input and responds to specific control sequences

echo "Escape test ready"

while true; do
    printf "> "

    if ! read -r line; then
        echo "EOF"
        break
    fi

    case "$line" in
        quit)
            echo "Goodbye"
            break
            ;;
        test)
            echo "Test response"
            ;;
        *)
            echo "Echo: $line"
            ;;
    esac
done

echo "Test finished"
