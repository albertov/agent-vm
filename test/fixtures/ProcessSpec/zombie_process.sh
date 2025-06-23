#!/usr/bin/env bash
# Create a zombie process for testing
echo "Creating zombie process..."

# Fork a child process that exits immediately
(
    echo "Child process starting"
    exit 0
) &

CHILD_PID=$!
echo "Child PID: $CHILD_PID"

# Sleep for a while without waiting for the child
# This creates a zombie until the parent exits
sleep 0.2

echo "Parent exiting, zombie should be cleaned up"
