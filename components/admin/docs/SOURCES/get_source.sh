#!/usr/bin/env bash

# Note: The current working directory is the component's SPEC folder

# The folder that will contain the .git/ and docs/ folders
DOCS_OHPC_FOLDER=docs-ohpc
# A folder that contains the full Git history of the project
# Full history is needed for the usage of `git archive` command
COMPLETE_OHPC_FOLDER=$(mktemp -d)

cleanup() {
    rm -rf ${DOCS_OHPC_FOLDER} ${COMPLETE_OHPC_FOLDER}
}
trap cleanup EXIT

# 1. Prepare
cleanup
mkdir -p ${DOCS_OHPC_FOLDER}

# 2. Fetch the full Git history so that `git describe` works later
git clone https://github.com/openhpc/ohpc ${COMPLETE_OHPC_FOLDER}
cp -r ${COMPLETE_OHPC_FOLDER}/.git ${DOCS_OHPC_FOLDER}

# 3. Copy the local docs/
cp -r ../../../../docs ${DOCS_OHPC_FOLDER}

# 4. Create docs-ohpc.tar
tar cf ../SOURCES/docs-ohpc.tar ${DOCS_OHPC_FOLDER}

# 5. Cleanup
cleanup
