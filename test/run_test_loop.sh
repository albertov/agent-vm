#!/bin/bash
# Run tests multiple times to check for flakiness
for i in {1..20}; do
    echo "Run $i:"
    if ! cabal test spec; then
        echo "Test failed on run $i"
        exit 1
    fi
done
echo "All 20 runs passed successfully"
