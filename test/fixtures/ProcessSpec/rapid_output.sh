#!/usr/bin/env bash
# Test script with rapid output
for i in {1..10}; do
    echo "stdout line $i"
    >&2 echo "stderr line $i"
done
exit 0
