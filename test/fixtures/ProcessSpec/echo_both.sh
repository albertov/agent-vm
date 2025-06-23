#!/bin/bash
# Test script that writes to both stdout and stderr
echo "Starting process..."
>&2 echo "Warning: This is stderr"
echo "Processing data..."
>&2 echo "Error: Something went wrong"
echo "Done!"
exit 0
