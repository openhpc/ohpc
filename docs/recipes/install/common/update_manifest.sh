#!/bin/bash

set -euo pipefail
set -x

# Check if running from a directory called "manifest"
current_dir=$(basename "$(pwd)")
if [ "$current_dir" != "manifest" ]; then
    echo "Error: This script must be run from a directory called 'manifest'"
    echo "Current directory: $current_dir"
    exit 1
fi

echo "Running manifest update from directory: $current_dir"

../common/listohpc 3.4
../common/listchanges
../common/build_tables.pl
../common/build_patterns.pl
../common/build_changelog.pl
