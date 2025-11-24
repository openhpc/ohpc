#!/usr/bin/env bash

# Copyright 2021 The Kubernetes Authors.
# Adapted for OpenHPC documentation
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -o errexit
set -o nounset
set -o pipefail

# Check if mdtoc is available
if ! command -v mdtoc &> /dev/null; then
    echo "mdtoc not found in PATH. Please install it first."
    exit 1
fi

# Process all markdown files passed as arguments
# If no arguments, process stdin
if [ $# -eq 0 ]; then
    echo "Usage: $0 <markdown-file> [<markdown-file>...]"
    exit 1
fi

# Update TOC in each file
for file in "$@"; do
    if [ ! -f "$file" ]; then
        echo "File not found: $file"
        continue
    fi

    echo "Updating TOC in $file"
    mdtoc --inplace --max-depth=3 "$file"
done
