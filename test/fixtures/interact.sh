#!/usr/bin/env bash

# Force unbuffered output - this is crucial when stdout is not a terminal
exec 1> >(stdbuf -oL cat)

# Interactive test script for testing bidirectional communication
# Commands:
#   echo <text> - Echo the text back
#   sleep <seconds> - Sleep for specified seconds
#   quit - Exit the script

echo "Interactive test script started. Commands: echo <text>, sleep <seconds>, quit"

while true; do
    # Print prompt
    printf "> "
    # Force flush stdout to ensure prompt is visible
    # This is important when stdout is not a terminal
    exec 1>&1

    # Read command and handle EOF (Ctrl+D)
    if ! read -r cmd args; then
        echo
        echo "EOF received, exiting..."
        break
    fi

    # Process commands
    case "$cmd" in
        echo)
            if [ -n "$args" ]; then
                echo "Echo: $args"
            else
                echo "Error: echo requires an argument"
            fi
            ;;
        sleep)
            if [[ "$args" =~ ^[0-9]+$ ]]; then
                echo "Sleeping for $args seconds..."
                sleep "$args"
                echo "Done sleeping"
            else
                echo "Error: sleep requires a numeric argument"
            fi
            ;;
        quit|exit)
            echo "Goodbye!"
            break
            ;;
        "")
            # Empty line, just show prompt again
            ;;
        *)
            echo "Unknown command: $cmd"
            echo "Available commands: echo <text>, sleep <seconds>, quit"
            ;;
    esac
done

echo "Interactive test script terminated"
