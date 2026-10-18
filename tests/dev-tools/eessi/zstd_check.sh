#!/bin/bash
# Load zstd from EESSI on the node executing this script, verify that the
# binary is located on CernVM-FS and run a compression round trip. The
# eessi test runs this on the head node and, through the resource
# manager, on a compute node to exercise the EESSI modules there.

set -e

EESSI_REPO="${EESSI_REPO:-/cvmfs/software.eessi.io}"
EESSI_VERSION="${EESSI_VERSION:-2026.06}"

if ! type module >/dev/null 2>&1; then
	. /etc/profile.d/modules.sh
fi

module load eessi
module load "EESSI/${EESSI_VERSION}"
module load zstd

zstd_bin="$(command -v zstd)"
echo "zstd on $(hostname): ${zstd_bin}"
case "${zstd_bin}" in
"${EESSI_REPO}"/*) ;;
*)
	echo "zstd is not located on CernVM-FS" >&2
	exit 1
	;;
esac

tmpdir="$(mktemp -d)"
trap 'rm -rf "${tmpdir}"' EXIT

zstd -q -o "${tmpdir}/input.zst" "$0"
zstd -q -d -o "${tmpdir}/output" "${tmpdir}/input.zst"
cmp "$0" "${tmpdir}/output"
echo "zstd round trip succeeded on $(hostname)"
