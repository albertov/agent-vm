#!/bin/bash
# Test script that writes to stderr
>&2 echo "Error message to stderr"
>&2 echo "Another error line"
exit 0
