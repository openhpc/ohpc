#!/bin/bash
#-----------------------------------------------------------------------
# gen_sources_checksum.sh - Generate a SHA-512 checksum manifest for
# the source files of an OpenHPC RPM spec.
#
# Usage:
#   gen_sources_checksum.sh <specfile>
#
# The script:
#   1. Parses all Source tags from <specfile> using "rpmspec --parse".
#   2. Extracts the basename of each Source URL/path.
#   3. Looks for matching files in the SOURCES/ directory (located
#      relative to the spec via the standard SPECS/../SOURCES layout).
#   4. Computes SHA-512 checksums in BSD-style "sha512sum --tag" format
#      and writes them to SOURCES/sources.
#
# The resulting manifest is consumed at build time by the
# macros.ohpc-source-verify RPM macro (shipped in ohpc-buildroot)
# which verifies every checksum before %prep runs.
#
# The manifest is also compatible with "sha512sum --check" for manual
# verification.
#-----------------------------------------------------------------------

set -euo pipefail

die() {
	echo "ERROR: $*" >&2
	exit 1
}

[[ $# -eq 1 ]] || die "usage: $(basename "$0") <specfile>"

specfile="$1"
[[ -f "${specfile}" ]] || die "spec file not found: ${specfile}"

# Derive the SOURCES directory relative to the spec file.
# Convention: SPECS/foo.spec -> SOURCES/
spec_dir="$(cd "$(dirname "${specfile}")" && pwd)"
sources_dir="${spec_dir}/../SOURCES"
[[ -d "${sources_dir}" ]] || die "SOURCES directory not found: ${sources_dir}"
sources_dir="$(cd "${sources_dir}" && pwd)"

manifest="${sources_dir}/sources"

# Collect Source file basenames from the parsed spec.
# "rpmspec --parse" expands all macros/conditionals so we see the
# final Source lines regardless of %if guards or %include files.
mapfile -t source_urls < <(
	rpmspec --parse "${specfile}" 2>/dev/null |
		grep -iE '^Source[0-9]*\s*:' |
		sed 's/^[^:]*:\s*//'
)

[[ ${#source_urls[@]} -gt 0 ]] || die "no Source tags found in ${specfile}"

tmpfile="$(mktemp)"
trap 'rm -f "${tmpfile}"' EXIT

count=0
for url in "${source_urls[@]}"; do
	base="$(basename "${url}")"
	# OHPC_macros is a shared file injected via %include and is not a
	# package-specific source; skip it to avoid false mismatches when
	# the global copy is updated independently.
	[[ "${base}" == "OHPC_macros" ]] && continue
	path="${sources_dir}/${base}"
	if [[ -f "${path}" ]]; then
		# Write BSD-style checksum with path stripped to bare filename
		sha512sum --tag "${path}" | sed "s|${sources_dir}/||" >>"${tmpfile}"
		((count++)) || true
	else
		echo "WARNING: source file not found, skipping: ${base}" >&2
	fi
done

[[ ${count} -gt 0 ]] || die "no source files found in ${sources_dir}"

mv "${tmpfile}" "${manifest}"
echo "Wrote ${count} checksum(s) to ${manifest}"
