#!/usr/bin/env bash

set -xe

# Note: The current working directory is the component's SPEC folder
OHPC_ROOT_FOLDER="../../../.."

# The folder that will contain the source tree for the tarball
DOCS_OHPC_FOLDER="docs-ohpc"

cleanup() {
    rm -rf "${DOCS_OHPC_FOLDER}"
}
trap cleanup EXIT

# 1. Prepare
cleanup
mkdir -p "${DOCS_OHPC_FOLDER}/docs"

# 2. Copy the Git metadata (used by mkdoc.py for version info)
cp -r "${OHPC_ROOT_FOLDER}/.git" "${DOCS_OHPC_FOLDER}"

# 3. Copy the new documentation system
cp -r "${OHPC_ROOT_FOLDER}/docs/install" "${DOCS_OHPC_FOLDER}/docs/"

# 4. Clean build artifacts that should not be in the tarball
rm -rf "${DOCS_OHPC_FOLDER}/docs/install/.venv"
rm -rf "${DOCS_OHPC_FOLDER}/docs/install/build"

# 5. Copy top-level doc files
cp "${OHPC_ROOT_FOLDER}/docs/ChangeLog" "${DOCS_OHPC_FOLDER}/docs/"
cp "${OHPC_ROOT_FOLDER}/docs/Release_Notes.txt" "${DOCS_OHPC_FOLDER}/docs/"

# 6. Create docs-ohpc.tar
tar cf ../SOURCES/docs-ohpc.tar "${DOCS_OHPC_FOLDER}"
