#!/usr/bin/env bash

set -xe

# Note: The current working directory is the component's SPEC folder
OHPC_ROOT_FOLDER="../../../.."

# The folder that will contain the .git/ and docs/ folders
DOCS_OHPC_FOLDER="docs-ohpc"

cleanup() {
    rm -rf "${DOCS_OHPC_FOLDER}"
    find "${OHPC_ROOT_FOLDER}/docs/recipes/install/" -name "vc.tex" -delete
}
trap cleanup EXIT

# 1. Prepare
cleanup
mkdir -p "${DOCS_OHPC_FOLDER}"

# 2. Copy the Git metadata
cp -r "${OHPC_ROOT_FOLDER}/.git" "${DOCS_OHPC_FOLDER}"

# 3. Copy the local docs/
cp -r "${OHPC_ROOT_FOLDER}/docs" "${DOCS_OHPC_FOLDER}"

# 4. Add dummy vc.tex to the docs
find "${DOCS_OHPC_FOLDER}"/docs/recipes/install/ -name "Makefile" -print0 | while IFS= read -r -d '' makefile
do
    folder=$(dirname "${makefile}")
    cat <<'EOF' > "${folder}"/vc.tex
%%% Define Git specific macros.
\gdef\GITHash{5ffbf3e2ed0ed558c1ae5672f7e5023298b7c2a9}%
\gdef\GITAbrHash{5ffbf3e}%
\gdef\GITParentHashes{}%
\gdef\GITAbrParentHashes{}%
\gdef\GITAuthorName{CI Docs author}%
\gdef\GITAuthorEmail{simple_ci@docs.com}%
\gdef\GITAuthorDate{2024-02-29 14:00:35 +0100}%
\gdef\GITCommitterName{CI Docs author}%
\gdef\GITCommitterEmail{simple_ci@docs.com}%
\gdef\GITCommitterDate{2024-02-29 14:00:35 +0100}%
%%% Define generic version control macros.
\gdef\VCRevision{\GITAbrHash}%
\gdef\VCAuthor{\GITAuthorName}%
\gdef\VCDateRAW{2024-02-29}%
\gdef\VCDateISO{2024-02-29}%
\gdef\VCDateTEX{2024/02/29}%
\gdef\VCTime{14:00:35 +0100}%
\gdef\VCModifiedText{\textcolor{red}{with local modifications!}}%
%%% Assume clean working copy.
\gdef\VCModified{0}%
\gdef\VCRevisionMod{\VCRevision}%
EOF

done

# 5. Create docs-ohpc.tar
tar cf ../SOURCES/docs-ohpc.tar "${DOCS_OHPC_FOLDER}"
