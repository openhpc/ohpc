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
#      and writes them to SOURCES/ohpc.checksums.
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

manifest="${sources_dir}/ohpc.checksums"

# The spec files include %{_sourcedir}/OHPC_macros, and OHPC_macros is in
# components/. Without the define, rpmspec looks for it in
# ~/rpmbuild/SOURCES and fails on a host that has no copy there.
components_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")/../components" && pwd)"

tmpfile="$(mktemp)"
rpmspec_errors="$(mktemp)"
trap 'rm -f "${tmpfile}" "${rpmspec_errors}"' EXIT

# Collect Source file basenames from the parsed spec.
# "rpmspec --parse" expands all macros/conditionals so we see the
# final Source lines regardless of %if guards or %include files.
# rpmspec warns about macros in the comments of some spec files. Print its
# messages only when it fails, so that a caller can treat any message on
# stderr as a problem with the manifest.
if ! parsed="$(rpmspec --parse --define "_sourcedir ${components_dir}" \
	"${specfile}" 2>"${rpmspec_errors}")"; then
	cat "${rpmspec_errors}" >&2
	die "rpmspec --parse failed for ${specfile}"
fi
mapfile -t source_urls < <(
	grep -iE '^Source[0-9]*\s*:' <<<"${parsed}" |
		sed 's/^[^:]*:\s*//'
)

[[ ${#source_urls[@]} -gt 0 ]] || die "no Source tags found in ${specfile}"

count=0
for url in "${source_urls[@]}"; do
	base="$(basename "${url}")"
	# OHPC_macros is a shared file injected via %include and is not a
	# package-specific source; skip it to avoid false mismatches when
	# the global copy is updated independently.
	# ohpc.checksums is the manifest itself (Source43); skip it.
	[[ "${base}" == "OHPC_macros" || "${base}" == "ohpc.checksums" ]] && continue
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
