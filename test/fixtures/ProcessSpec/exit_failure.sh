#!/bin/bash
# Test script that exits with error code
echo "Attempting operation..."
>&2 echo "FATAL: Operation failed!"
exit 1
