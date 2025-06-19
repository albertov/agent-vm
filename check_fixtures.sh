#!/bin/bash
# Check permissions of fixture scripts
echo "Checking fixture script permissions..."
for script in test/fixtures/ProcessSpec/*.sh; do
    if [[ ! -x "$script" ]]; then
        echo "$script is not executable"
        chmod a+x "$script"
    else
        echo "$script is already executable"
    fi
done
