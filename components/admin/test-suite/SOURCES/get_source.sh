#!/usr/bin/env bash

set -xe

# Note: The current working directory is the component's SPEC folder
OHPC_ROOT_FOLDER="../../../.."

# The folder that will contain the .git/ and docs/ folders
TESTS_OHPC_FOLDER=tests-ohpc

cleanup() {
    rm -rf "${TESTS_OHPC_FOLDER}"
}
trap cleanup EXIT

# 1. Prepare
cleanup
mkdir -p "${TESTS_OHPC_FOLDER}"

# 2. Copy the local docs/
cp -r "${OHPC_ROOT_FOLDER}/tests" "${TESTS_OHPC_FOLDER}"

# 3. Create tests-ohpc.tar
tar cf ../SOURCES/tests-ohpc.tar "${TESTS_OHPC_FOLDER}"
