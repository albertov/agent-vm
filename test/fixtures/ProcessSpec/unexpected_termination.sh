#!/usr/bin/env bash
# Simulates a process that terminates unexpectedly

echo "Process starting normally..."
echo "Doing some work..."

# Simulate sudden termination with SIGKILL (can't be caught)
# We'll use kill -9 on ourselves after a short delay
sleep 0.1

echo "About to terminate unexpectedly..."

# Force kill ourselves with SIGKILL
kill -9 $$
