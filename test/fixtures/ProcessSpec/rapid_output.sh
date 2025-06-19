#!/usr/bin/env bash
# Test script with rapid output
for i in {1..10}; do
    echo "stdout line $i"
    >&2 echo "stderr line $i"
    sleep 0.01
done
exit 0
