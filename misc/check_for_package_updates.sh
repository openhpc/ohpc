#!/bin/bash

# Exit on any error, undefined variables, and pipe failures
set -euo pipefail

# Script to check for newer GitHub, GitLab, PyPI, and JSC perftools releases of OpenHPC packages
SCRIPT_NAME="$(basename "$0")"
readonly SCRIPT_NAME
TEMP_DIR="$(mktemp -d)"
readonly TEMP_DIR
readonly RESULTS_FILE="${TEMP_DIR}/results.txt"

# Colors for output
readonly RED='\033[0;31m'
readonly GREEN='\033[0;32m'
readonly YELLOW='\033[1;33m'
readonly BLUE='\033[0;34m'
readonly NC='\033[0m' # No Color

# Configuration
GITHUB_TOKEN="${GITHUB_TOKEN:-}"
VERBOSE=0
CHECK_PRERELEASES=0
OUTPUT_FORMAT="table"
NO_GLOW=0

# Cleanup function
# shellcheck disable=SC2329
cleanup() {
	rm -rf "${TEMP_DIR}"
}
trap cleanup EXIT

# Logging functions
# shellcheck disable=SC2329
log() {
	echo "[$(date '+%Y-%m-%d %H:%M:%S')] $*" >&2
}

log_error() {
	echo -e "${RED}[ERROR]${NC} $*" >&2
}

log_warn() {
	echo -e "${YELLOW}[WARN]${NC} $*" >&2
}

log_info() {
	echo -e "${BLUE}[INFO]${NC} $*" >&2
}

# shellcheck disable=SC2329
log_success() {
	echo -e "${GREEN}[SUCCESS]${NC} $*" >&2
}

# Debug logging functions (only output when verbose mode is enabled)
debug_info() {
	if [[ ${VERBOSE} -eq 1 ]]; then
		log_info "$@"
	fi
}

debug_warn() {
	if [[ ${VERBOSE} -eq 1 ]]; then
		log_warn "$@"
	fi
}

# Usage information
usage() {
	cat <<EOF
Usage: ${SCRIPT_NAME} [OPTIONS]

Check for newer GitHub, GitLab, PyPI, and JSC perftools releases of OpenHPC packages
by analyzing spec files.

OPTIONS:
    -v, --verbose           Enable verbose output
    -p, --prereleases       Include pre-releases in version checks (GitHub/JSC perftools)
    -o, --output FORMAT     Output format: table, json, markdown (default: table)
    -t, --token TOKEN       GitHub API token (or set GITHUB_TOKEN env var)
    --no-glow               Disable glow formatting for markdown output
    -h, --help              Show this help message

EXAMPLES:
    ${SCRIPT_NAME}                          # Basic check with table output
    ${SCRIPT_NAME} -v -p                    # Verbose with pre-releases
    ${SCRIPT_NAME} -o json                  # JSON output
    ${SCRIPT_NAME} -o markdown              # Markdown output (with glow if installed)
    ${SCRIPT_NAME} -o markdown --no-glow    # Markdown output (plain text)
    ${SCRIPT_NAME} -t ghp_token123          # With GitHub token

NOTES:
    - Requires rpmspec and rpmdev-vercmp tools
    - Supports GitHub (github.com), GitLab (any instance), PyPI, and JSC perftools
    - GitHub API rate limits apply (60 requests/hour without token)
    - Use GITHUB_TOKEN environment variable for higher rate limits (5000 requests/hour)
    - Get a token at: https://github.com/settings/tokens (no special permissions needed)
    - GitLab repositories use public API endpoints (no authentication required)
    - PyPI repositories use public API endpoints (no authentication required)
    - JSC perftools packages (perftools.pages.jsc.fz-juelich.de) use public pages

EOF
}

# Check required tools
check_dependencies() {
	local missing_tools=()

	for tool in rpmspec rpmdev-vercmp curl jq; do
		if ! command -v "${tool}" &>/dev/null; then
			missing_tools+=("${tool}")
		fi
	done

	if [[ ${#missing_tools[@]} -gt 0 ]]; then
		log_error "Missing required tools: ${missing_tools[*]}"
		log_error "Please install: rpm-build rpmdevtools curl jq"
		exit 1
	fi
}

# Setup RPM environment
setup_rpm_environment() {
	local sourcedir
	sourcedir="$(rpm --eval '%{_sourcedir}')"

	if [[ ! -e "components/OHPC_macros" ]]; then
		log_error "This script must be run from the OpenHPC top-level directory"
		exit 1
	fi

	# Copy OHPC_macros if needed
	if [[ ! -e "${sourcedir}/OHPC_macros" ]]; then
		log_info "Copying OHPC_macros to ${sourcedir}"
		cp components/OHPC_macros "${sourcedir}/OHPC_macros" || {
			log_error "Failed to copy OHPC_macros"
			exit 1
		}
	fi

	# Check if they match
	if ! cmp -s "${sourcedir}/OHPC_macros" components/OHPC_macros; then
		log_warn "OHPC_macros files differ between ${sourcedir} and components/"
	fi
}

# Extract package information using rpmspec
extract_package_info() {
	local spec_file="$1"
	local flags=()

	# Handle Fedora-specific defines like get_source.sh
	local distro
	distro="$(rpm --eval '0%{?fedora}')"
	if [[ "${distro}" != "0" ]]; then
		flags=(--undefine fedora --define "rhel 10")
	fi

	# Get the directory and basename for proper rpmspec parsing
	local spec_dir spec_base
	spec_dir="$(dirname "${spec_file}")"
	spec_base="$(basename "${spec_file}")"

	# Change to spec directory for parsing
	pushd "${spec_dir}" >/dev/null || {
		debug_warn "Failed to change to directory ${spec_dir}"
		return 1
	}

	# Extract package information using rpmspec --parse like get_source.sh
	local parsed_output name version source0

	# Parse the spec file and extract information
	# Capture both stdout and stderr to check for architecture errors
	local temp_output temp_errors
	temp_output="$(mktemp)"
	temp_errors="$(mktemp)"

	rpmspec --parse --define '_sourcedir ../../..' "${flags[@]}" "${spec_base}" >"${temp_output}" 2>"${temp_errors}"
	local parse_result=$?

	parsed_output="$(cat "${temp_output}")"
	local error_content
	error_content="$(cat "${temp_errors}")"

	# Clean up temp files
	rm -f "${temp_output}" "${temp_errors}"

	# Handle parsing errors
	if [[ ${parse_result} -ne 0 || -z "${parsed_output}" ]]; then
		if [[ "${error_content}" == *"No compatible architectures found for build"* ]]; then
			popd >/dev/null
			debug_info "Skipping $(basename "${spec_file}"): architecture not compatible"
			return 2 # Special return code for "not processable"
		else
			popd >/dev/null
			debug_warn "Failed to parse ${spec_file}: ${error_content}"
			return 1
		fi
	fi

	# Extract name (first package name from parsed output)
	name="$(echo "${parsed_output}" | grep -m1 '^Name:' | awk '{print $2}' | head -1)"

	# Extract version
	version="$(echo "${parsed_output}" | grep -m1 '^Version:' | awk '{print $2}' | head -1)"

	# Extract source URL - only consider URLs that start with http
	source0="$(echo "${parsed_output}" | grep -E '^Source[0-9]*:' | awk '{print $2}' | grep '^http' | head -1)"

	popd >/dev/null

	# Validate extracted information
	if [[ -z "${name}" || -z "${version}" ]]; then
		debug_warn "Incomplete package info for ${spec_file}: name='${name}' version='${version}'"
		return 1
	fi

	# Check if there's no HTTP source tag
	if [[ -z "${source0}" ]]; then
		debug_info "Skipping ${name}: no HTTP upstream source defined"
		return 2 # Special return code for "no source"
	fi

	echo "${name}|${version}|${source0}"
}

# Parse GitHub repository from URL
parse_github_repo() {
	local url="$1"

	# Match various GitHub URL patterns
	if [[ "${url}" =~ https://github\.com/([^/]+)/([^/]+)/ ]]; then
		local owner="${BASH_REMATCH[1]}"
		local repo="${BASH_REMATCH[2]}"
		echo "${owner}/${repo}"
		return 0
	fi

	return 1
}

# Parse GitLab repository from URL
parse_gitlab_repo() {
	local url="$1"

	# Match various GitLab URL patterns
	# Examples:
	# https://gitlab.com/charliecloud/charliecloud/-/package_files/...
	# https://gitlab.inria.fr/scotch/scotch/-/archive/...
	if [[ "${url}" =~ https://([^/]+)/([^/]+)/([^/]+)/- ]]; then
		local hostname="${BASH_REMATCH[1]}"
		local owner="${BASH_REMATCH[2]}"
		local repo="${BASH_REMATCH[3]}"
		echo "${hostname}|${owner}/${repo}"
		return 0
	fi

	return 1
}

# Parse PyPI package name from URL
parse_pypi_package() {
	local url="$1"

	# Match PyPI URL patterns
	# Examples:
	# https://files.pythonhosted.org/packages/source/C/Cython/Cython-3.0.0.tar.gz
	# https://pypi.io/packages/source/e/easybuild/easybuild-4.8.2.tar.gz
	if [[ "${url}" =~ https://(files\.pythonhosted\.org|pypi\.io)/packages/source/[^/]+/([^/]+)/ ]]; then
		local package_name="${BASH_REMATCH[2]}"
		echo "${package_name}"
		return 0
	fi

	return 1
}

# Parse JSC perftools project name from URL
parse_jsc_perftools_package() {
	local url="$1"

	# Match JSC perftools URL patterns
	# Examples:
	# https://perftools.pages.jsc.fz-juelich.de/cicd/scorep/tags/scorep-9.0/scorep-9.0.tar.gz
	# http://perftools.pages.jsc.fz-juelich.de/cicd/opari2/tags/opari2-2.0.8/opari2-2.0.8.tar.gz
	# https://perftools.pages.jsc.fz-juelich.de/cicd/cubelib/tags/cubelib-4.9/cubelib-4.9.tar.gz
	if [[ "${url}" =~ https?://perftools\.pages\.jsc\.fz-juelich\.de/cicd/([^/]+)/ ]]; then
		local project_name="${BASH_REMATCH[1]}"
		echo "${project_name}"
		return 0
	fi

	return 1
}

# Get GitLab project ID from hostname and path
get_gitlab_project_id() {
	local hostname="$1"
	local project_path="$2"                 # format: owner/repo
	local search_name="${project_path##*/}" # extract repo name

	debug_info "Searching for GitLab project ID for ${project_path} on ${hostname}"

	local search_url="https://${hostname}/api/v4/projects?search=${search_name}"
	local projects

	projects="$(curl -s "${search_url}" 2>/dev/null)" || {
		debug_warn "Failed to search for GitLab project ${project_path} on ${hostname}"
		return 1
	}

	# Check if we got an error
	if echo "${projects}" | jq -e '.message' &>/dev/null; then
		local message
		message="$(echo "${projects}" | jq -r '.message')"
		debug_warn "GitLab API error for ${project_path}: ${message}"
		return 1
	fi

	# Filter by path_with_namespace to match the organization/project pattern
	local project_id
	project_id="$(echo "${projects}" | jq -r ".[] | select(.path_with_namespace == \"${project_path}\") | .id" | head -1)" || {
		debug_warn "Failed to parse GitLab project search results for ${project_path}"
		return 1
	}

	if [[ -z "${project_id}" || "${project_id}" == "null" ]]; then
		debug_warn "No GitLab project found for ${project_path} on ${hostname}"
		return 1
	fi

	echo "${project_id}"
}

# Get latest release from GitLab API
get_latest_gitlab_release() {
	local hostname="$1"
	local project_id="$2"
	local api_url="https://${hostname}/api/v4/projects/${project_id}/repository/tags"

	debug_info "Fetching GitLab tags from ${api_url}"

	# Get tags (no authentication needed for public repos)
	local tags
	tags="$(curl -s "${api_url}" 2>/dev/null)" || {
		debug_warn "Failed to fetch tags for GitLab project ${project_id} on ${hostname}"
		return 1
	}

	# Check if we got an error
	if echo "${tags}" | jq -e '.message' &>/dev/null; then
		local message
		message="$(echo "${tags}" | jq -r '.message')"
		debug_warn "GitLab API error for project ${project_id}: ${message}"
		return 1
	fi

	# Check if tags list is empty
	if echo "${tags}" | jq -e 'length == 0' &>/dev/null; then
		debug_info "No tags found for GitLab project ${project_id} on ${hostname}"
		return 1
	fi

	# Get the latest tag (first in the list, as GitLab returns them sorted by creation date desc)
	local latest_tag
	latest_tag="$(echo "${tags}" | jq -r '.[0].name' 2>/dev/null)" || {
		debug_warn "Failed to parse tags for GitLab project ${project_id}"
		return 1
	}

	if [[ "${latest_tag}" == "null" || -z "${latest_tag}" ]]; then
		debug_warn "No suitable tags found for GitLab project ${project_id}"
		return 1
	fi

	echo "${latest_tag}"
}

# Get latest version from PyPI API
get_latest_pypi_version() {
	local package_name="$1"
	local api_url="https://pypi.org/pypi/${package_name}/json"

	debug_info "Fetching PyPI package info from ${api_url}"

	# Get package info (no authentication needed for public packages)
	local package_info
	package_info="$(curl -s "${api_url}" 2>/dev/null)" || {
		debug_warn "Failed to fetch PyPI package info for ${package_name}"
		return 1
	}

	# Check if we got an error
	if echo "${package_info}" | jq -e '.message' &>/dev/null; then
		local message
		message="$(echo "${package_info}" | jq -r '.message')"
		debug_warn "PyPI API error for ${package_name}: ${message}"
		return 1
	fi

	# Get the latest version
	local latest_version
	latest_version="$(echo "${package_info}" | jq -r '.info.version' 2>/dev/null)" || {
		debug_warn "Failed to parse PyPI version for ${package_name}"
		return 1
	}

	if [[ "${latest_version}" == "null" || -z "${latest_version}" ]]; then
		debug_warn "No version found for PyPI package ${package_name}"
		return 1
	fi

	echo "${latest_version}"
}

# Get latest version from JSC perftools overview page
get_latest_jsc_perftools_version() {
	local project_name="$1"
	local base_url="https://perftools.pages.jsc.fz-juelich.de/cicd/${project_name}/"

	debug_info "Fetching JSC perftools package info from ${base_url}"

	# Fetch the overview page
	local page_content
	page_content="$(curl -sL "${base_url}" 2>/dev/null)" || {
		debug_warn "Failed to fetch JSC perftools page for ${project_name}"
		return 1
	}

	# Check if we got valid HTML content
	if [[ -z "${page_content}" || ! "${page_content}" =~ \<html ]]; then
		debug_warn "Invalid response from JSC perftools page for ${project_name}"
		return 1
	fi

	# Extract the latest version from the Tags section
	# The first tarball link after "Tags" header contains the latest version
	# Pattern: href="tags/<project>-<version>/<project>-<version>.tar.gz"
	# We need to find version, which may include dots and dashes
	# Skip pre-release versions (containing -rc, -alpha, -beta) unless prereleases enabled
	local version_pattern latest_version

	if [[ ${CHECK_PRERELEASES} -eq 1 ]]; then
		# Include all versions including pre-releases
		version_pattern="tags/${project_name}-([^/]+)/${project_name}-[^\"]+\.tar\.gz"
	else
		# Exclude pre-release versions (those with -rc, -alpha, -beta, etc.)
		# First, get all versions from the tags section
		version_pattern="tags/${project_name}-([^/]+)/${project_name}-[^\"]+\.tar\.gz"
	fi

	# Extract all version tags from the page
	local all_versions
	all_versions="$(echo "${page_content}" | grep -oE "${version_pattern}" | head -20)"

	if [[ -z "${all_versions}" ]]; then
		debug_warn "No version tags found for JSC perftools package ${project_name}"
		return 1
	fi

	# Parse versions and find the latest stable one
	while IFS= read -r match; do
		# Extract version from the match
		if [[ "${match}" =~ tags/${project_name}-([^/]+)/ ]]; then
			local version="${BASH_REMATCH[1]}"

			# Skip pre-release versions unless enabled
			if [[ ${CHECK_PRERELEASES} -eq 0 ]]; then
				if [[ "${version}" =~ -(rc|alpha|beta|pre|dev)[0-9]*$ ]]; then
					debug_info "Skipping pre-release version: ${version}"
					continue
				fi
			fi

			# First matching non-prerelease version is the latest
			latest_version="${version}"
			break
		fi
	done <<<"${all_versions}"

	if [[ -z "${latest_version}" ]]; then
		debug_warn "No suitable version found for JSC perftools package ${project_name}"
		return 1
	fi

	echo "${latest_version}"
}

# Get latest version from GNU FTP directory listing
get_latest_gnu_version() {
	local project="$1" # e.g., "gcc", "gmp", "mpc", "mpfr"
	local base_url="https://ftpmirror.gnu.org/gnu/${project}/"

	debug_info "Checking GNU ${project} directory listing at ${base_url}"

	# Fetch directory listing
	local listing
	listing="$(curl -sL "${base_url}" 2>/dev/null)" || {
		debug_warn "Failed to fetch directory listing for GNU ${project}"
		return 1
	}

	# Extract version directories/files based on project
	local latest_version
	case "${project}" in
	"gcc")
		# Extract gcc-X.Y.Z/ directories from HTML listing, find highest version
		latest_version="$(echo "${listing}" | grep -oE 'href="gcc-[0-9]+\.[0-9]+\.[0-9]+/"' |
			sed 's/href="gcc-//; s|/"||' | sort -V | tail -1)"
		;;
	"gmp" | "mpc" | "mpfr")
		# Extract project-X.Y.Z.tar.* files from HTML listing, find highest version
		latest_version="$(echo "${listing}" | grep -oE "href=\"${project}-[0-9]+\.[0-9]+(\.[0-9]+)?\.(tar\.(gz|bz2|xz)|tgz)\"" |
			sed "s/href=\"${project}-//; s/\.tar\..*//" | sort -V | tail -1)"
		;;
	*)
		debug_warn "Unknown GNU project: ${project}"
		return 1
		;;
	esac

	if [[ -z "${latest_version}" ]]; then
		debug_warn "No version found for GNU ${project}"
		return 1
	fi

	echo "${latest_version}"
}

# Helper function to check a single GNU component
check_gnu_component() {
	local component_name="$1"   # e.g., "gnu15-compilers"
	local component_suffix="$2" # e.g., "gcc", "gmp", "mpc", "mpfr"
	local current_version="$3"  # current version from spec file
	local gnu_project_name="$4" # project name for get_latest_gnu_version

	local latest_version
	# Temporarily disable exit-on-error for command substitution
	set +e
	latest_version="$(get_latest_gnu_version "${gnu_project_name}")"
	local gnu_result=$?
	set -e

	if [[ ${gnu_result} -ne 0 ]]; then
		echo "${component_name}-${component_suffix}|${current_version}|ERROR|Failed to fetch ${gnu_project_name^^} releases|gnu.org/${gnu_project_name}" >>"${RESULTS_FILE}"
		return 1
	fi

	local comparison
	# Temporarily disable exit-on-error for command substitution
	set +e
	comparison="$(compare_versions "${current_version}" "${latest_version}")"
	set -e

	local status
	case "${comparison}" in
	"older") status="UPDATE_AVAILABLE" ;;
	"same") status="UP_TO_DATE" ;;
	"newer") status="AHEAD" ;;
	*) status="UNKNOWN" ;;
	esac

	echo "${component_name}-${component_suffix}|${current_version}|${latest_version}|${status}|gnu.org/${gnu_project_name}" >>"${RESULTS_FILE}"
	return 0
}

# Check for GNU compilers updates
check_gnu_compilers() {
	local spec_file="$1"

	debug_info "Processing GNU compilers spec file"

	# Extract current versions from spec file
	local spec_content
	spec_content="$(cat "${spec_file}")"

	# Extract default compiler family from OHPC_macros
	local default_compiler_family
	default_compiler_family="$(grep "compiler_family gnu" components/OHPC_macros | head -1 | sed 's/.*compiler_family \(gnu[0-9]*\).*/\1/')"

	if [[ -z "${default_compiler_family}" || ! "${default_compiler_family}" =~ ^gnu[0-9]+$ ]]; then
		debug_warn "Could not determine default compiler family from OHPC_macros, defaulting to gnu15"
		default_compiler_family="gnu15"
	fi

	# Extract version number from compiler family (e.g., gnu15 -> 15)
	local gnu_ver="${default_compiler_family#gnu}"

	debug_info "Using default compiler family: ${default_compiler_family} (version ${gnu_ver})"

	# Only check the default GNU compiler version
	{
		local gcc_version gmp_version mpc_version mpfr_version

		# Extract current versions from spec file
		gcc_version="$(echo "${spec_content}" | grep "^%global gnu${gnu_ver}_version" | awk '{print $3}')"
		gmp_version="$(echo "${spec_content}" | grep "^%global gnu${gnu_ver}_gmp_version" | awk '{print $3}')"
		mpc_version="$(echo "${spec_content}" | grep "^%global gnu${gnu_ver}_mpc_version" | awk '{print $3}')"
		mpfr_version="$(echo "${spec_content}" | grep "^%global gnu${gnu_ver}_mpfr_version" | awk '{print $3}')"

		if [[ -z "${gcc_version}" ]]; then
			debug_info "No GNU ${gnu_ver} version found in spec file"
			return 0
		fi

		debug_info "GNU ${gnu_ver}: GCC ${gcc_version}, GMP ${gmp_version}, MPC ${mpc_version}, MPFR ${mpfr_version}"

		# Check for updates for each component
		local component_name="gnu${gnu_ver}-compilers"

		# Check GCC
		check_gnu_component "${component_name}" "gcc" "${gcc_version}" "gcc" || return 0

		# Check GMP
		check_gnu_component "${component_name}" "gmp" "${gmp_version}" "gmp" || return 0

		# Check MPC
		check_gnu_component "${component_name}" "mpc" "${mpc_version}" "mpc" || return 0

		# Check MPFR
		check_gnu_component "${component_name}" "mpfr" "${mpfr_version}" "mpfr" || return 0

		debug_info "GNU ${gnu_ver} (default) check completed"
	}

	return 0
}

# Get latest release from GitHub API
get_latest_github_release() {
	local repo="$1"
	local headers=()

	if [[ -n "${GITHUB_TOKEN}" ]]; then
		headers=(-H "Authorization: token ${GITHUB_TOKEN}")
	else
		# Add delay to avoid rate limiting when no token is provided
		sleep 1
	fi

	# Try /releases/latest endpoint first (unless checking prereleases)
	# This returns what GitHub considers the "latest" release, which is more
	# accurate than sorting by creation date (patch releases on older branches
	# can be created after newer major releases)
	if [[ ${CHECK_PRERELEASES} -eq 0 ]]; then
		local latest_url="https://api.github.com/repos/${repo}/releases/latest"
		local latest_response
		latest_response="$(curl -s "${headers[@]}" "${latest_url}" 2>/dev/null)"

		# Check for rate limiting
		if echo "${latest_response}" | jq -e '.message' &>/dev/null; then
			local message
			message="$(echo "${latest_response}" | jq -r '.message')"
			if [[ "${message}" == *"rate limit"* ]]; then
				log_warn "GitHub API rate limit exceeded. Use GITHUB_TOKEN environment variable for higher limits."
				return 2 # Special return code for rate limiting
			fi
			# Other errors (e.g., no releases) - fall through to releases list
			debug_info "Latest endpoint failed for ${repo}: ${message}"
		else
			local latest_tag
			latest_tag="$(echo "${latest_response}" | jq -r '.tag_name' 2>/dev/null)"
			if [[ -n "${latest_tag}" && "${latest_tag}" != "null" ]]; then
				echo "${latest_tag}"
				return 0
			fi
		fi
	fi

	# Fall back to releases list (needed for prereleases or if /latest fails)
	local api_url="https://api.github.com/repos/${repo}/releases"
	local releases
	releases="$(curl -s "${headers[@]}" "${api_url}" 2>/dev/null)" || {
		debug_warn "Failed to fetch releases for ${repo}"
		return 1
	}

	# Check if we got rate limited or an error
	if echo "${releases}" | jq -e '.message' &>/dev/null; then
		local message
		message="$(echo "${releases}" | jq -r '.message')"
		if [[ "${message}" == *"rate limit"* ]]; then
			log_warn "GitHub API rate limit exceeded. Use GITHUB_TOKEN environment variable for higher limits."
			return 2 # Special return code for rate limiting
		fi
		debug_warn "GitHub API error for ${repo}: ${message}"
		return 1
	fi

	# Check if releases list is empty
	if echo "${releases}" | jq -e 'length == 0' &>/dev/null; then
		debug_info "No releases found for ${repo}"
		return 1
	fi

	# Find the latest prerelease (only used when CHECK_PRERELEASES=1)
	local latest_tag
	latest_tag="$(echo "${releases}" | jq -r '.[0].tag_name' 2>/dev/null)" || {
		debug_warn "Failed to parse releases for ${repo}"
		return 1
	}

	if [[ "${latest_tag}" == "null" || -z "${latest_tag}" ]]; then
		debug_warn "No suitable releases found for ${repo}"
		return 1
	fi

	echo "${latest_tag}"
}

# Normalize version strings for comparison
normalize_version() {
	local version="$1"

	# Project-specific version parsers
	# trilinos: trilinos-release-16-1-0 → 16.1.0
	if [[ "${version}" =~ trilinos-release-([0-9]+)-([0-9]+)-([0-9]+) ]]; then
		version="${BASH_REMATCH[1]}.${BASH_REMATCH[2]}.${BASH_REMATCH[3]}"
	# imb: IMB-v2021.10 → 2021.10
	elif [[ "${version}" =~ IMB-v(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# munge: munge-0.5.16 → 0.5.16
	elif [[ "${version}" =~ munge-(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# genders: genders-1-32-1 → 1.32 (drop last component)
	elif [[ "${version}" =~ genders-([0-9]+)-([0-9]+)-([0-9]+) ]]; then
		version="${BASH_REMATCH[1]}.${BASH_REMATCH[2]}"
	# conman: conman-0.3.1 → 0.3.1
	elif [[ "${version}" =~ conman-(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# pdsh: pdsh-2.35 → 2.35
	elif [[ "${version}" =~ pdsh-(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# llvm: llvmorg-21.1.3 → 21.1.3
	elif [[ "${version}" =~ llvmorg-(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# hdf5/phdf5: hdf5_1.14.6 → 1.14.6
	elif [[ "${version}" =~ hdf5_(.+) ]]; then
		version="${BASH_REMATCH[1]}"
	# Remove common prefixes like 'v', 'release-', etc.
	else
		version="${version#v}"
		version="${version#release-}"
		version="${version#rel-}"
	fi

	echo "${version}"
}

# Compare versions using rpmdev-vercmp
compare_versions() {
	local current="$1"
	local latest="$2"

	# Normalize versions
	current="$(normalize_version "${current}")"
	latest="$(normalize_version "${latest}")"

	# Use rpmdev-vercmp: returns 0 if equal, 11 if first > second, 12 if first < second
	rpmdev-vercmp "${current}" "${latest}" &>/dev/null
	local result=$?

	case ${result} in
	0) echo "same" ;;    # Versions are equal
	11) echo "newer" ;;  # Current is newer than latest
	12) echo "older" ;;  # Current is older than latest (update available)
	*) echo "unknown" ;; # Comparison failed
	esac
}

# Process a single spec file
process_spec_file() {
	local spec_file="$1"

	debug_info "Processing $(basename "${spec_file}")"

	# Special handling for GNU compilers spec file
	if [[ "${spec_file}" == */gnu-compilers/SPECS/gnu-compilers.spec ]]; then
		debug_info "Detected GNU compilers spec file - using special handling"
		check_gnu_compilers "${spec_file}"
		return $?
	fi

	# Extract package information
	local pkg_info extract_result

	# Temporarily disable exit-on-error for command substitution
	set +e
	pkg_info="$(extract_package_info "${spec_file}")"
	extract_result=$?
	set -e

	if [[ ${extract_result} -eq 2 ]]; then
		# No HTTP source or architecture not compatible - skip this file
		return 0
	elif [[ ${extract_result} -ne 0 ]]; then
		# Other error - fail
		return 1
	fi

	IFS='|' read -r name current_version source0 <<<"${pkg_info}"

	# Check if source is from GitHub
	local github_repo gitlab_info github_result

	# Temporarily disable exit-on-error for command substitution
	set +e
	github_repo="$(parse_github_repo "${source0}")"
	github_result=$?
	set -e

	if [[ ${github_result} -eq 0 ]]; then
		debug_info "Checking GitHub repo: ${github_repo}"

		# Get latest release
		local latest_tag result_code

		# Temporarily disable exit-on-error for command substitution
		set +e
		latest_tag="$(get_latest_github_release "${github_repo}")"
		result_code=$?
		set -e

		if [[ ${result_code} -eq 2 ]]; then
			# Rate limited - stop processing more packages to avoid further rate limiting
			echo "${name}|${current_version}|RATE_LIMITED|GitHub API rate limit exceeded|${github_repo}" >>"${RESULTS_FILE}"
			log_warn "Rate limit reached. Stopping further GitHub API requests."
			return 2 # Signal to stop processing
		elif [[ ${result_code} -ne 0 ]]; then
			echo "${name}|${current_version}|ERROR|Failed to fetch releases|${github_repo}" >>"${RESULTS_FILE}"
			return 0
		fi

		# Compare versions
		local comparison
		# Temporarily disable exit-on-error for command substitution
		set +e
		comparison="$(compare_versions "${current_version}" "${latest_tag}")"
		set -e

		# Determine status
		local status
		case "${comparison}" in
		"older") status="UPDATE_AVAILABLE" ;;
		"same") status="UP_TO_DATE" ;;
		"newer") status="AHEAD" ;;
		*) status="UNKNOWN" ;;
		esac

		# Write result
		echo "${name}|${current_version}|${latest_tag}|${status}|${github_repo}" >>"${RESULTS_FILE}"

		debug_info "Result: ${name} ${current_version} -> ${latest_tag} (${status})"
		return 0
	fi

	# Check if source is from GitLab
	local gitlab_result

	# Temporarily disable exit-on-error for command substitution
	set +e
	gitlab_info="$(parse_gitlab_repo "${source0}")"
	gitlab_result=$?
	set -e

	if [[ ${gitlab_result} -eq 0 ]]; then
		IFS='|' read -r hostname project_path <<<"${gitlab_info}"
		debug_info "Checking GitLab repo: ${project_path} on ${hostname}"

		# Get project ID
		local project_id project_id_result
		# Temporarily disable exit-on-error for command substitution
		set +e
		project_id="$(get_gitlab_project_id "${hostname}" "${project_path}")"
		project_id_result=$?
		set -e

		if [[ ${project_id_result} -ne 0 ]]; then
			echo "${name}|${current_version}|ERROR|Failed to get GitLab project ID|${hostname}/${project_path}" >>"${RESULTS_FILE}"
			return 0
		fi

		debug_info "Found GitLab project ID: ${project_id}"

		# Get latest tag
		local latest_tag tag_result
		# Temporarily disable exit-on-error for command substitution
		set +e
		latest_tag="$(get_latest_gitlab_release "${hostname}" "${project_id}")"
		tag_result=$?
		set -e

		if [[ ${tag_result} -ne 0 ]]; then
			echo "${name}|${current_version}|ERROR|Failed to fetch GitLab tags|${hostname}/${project_path}" >>"${RESULTS_FILE}"
			return 0
		fi

		# Compare versions
		local comparison
		# Temporarily disable exit-on-error for command substitution
		set +e
		comparison="$(compare_versions "${current_version}" "${latest_tag}")"
		set -e

		# Determine status
		local status
		case "${comparison}" in
		"older") status="UPDATE_AVAILABLE" ;;
		"same") status="UP_TO_DATE" ;;
		"newer") status="AHEAD" ;;
		*) status="UNKNOWN" ;;
		esac

		# Write result
		echo "${name}|${current_version}|${latest_tag}|${status}|${hostname}/${project_path}" >>"${RESULTS_FILE}"

		debug_info "Result: ${name} ${current_version} -> ${latest_tag} (${status})"
		return 0
	fi

	# Check if source is from PyPI
	local pypi_package pypi_result

	# Temporarily disable exit-on-error for command substitution
	set +e
	pypi_package="$(parse_pypi_package "${source0}")"
	pypi_result=$?
	set -e

	if [[ ${pypi_result} -eq 0 ]]; then
		debug_info "Checking PyPI package: ${pypi_package}"

		# Get latest version
		local latest_version version_result
		# Temporarily disable exit-on-error for command substitution
		set +e
		latest_version="$(get_latest_pypi_version "${pypi_package}")"
		version_result=$?
		set -e

		if [[ ${version_result} -ne 0 ]]; then
			echo "${name}|${current_version}|ERROR|Failed to fetch PyPI version|pypi.org/project/${pypi_package}" >>"${RESULTS_FILE}"
			return 0
		fi

		# Compare versions
		local comparison
		# Temporarily disable exit-on-error for command substitution
		set +e
		comparison="$(compare_versions "${current_version}" "${latest_version}")"
		set -e

		# Determine status
		local status
		case "${comparison}" in
		"older") status="UPDATE_AVAILABLE" ;;
		"same") status="UP_TO_DATE" ;;
		"newer") status="AHEAD" ;;
		*) status="UNKNOWN" ;;
		esac

		# Write result
		echo "${name}|${current_version}|${latest_version}|${status}|pypi.org/project/${pypi_package}" >>"${RESULTS_FILE}"

		debug_info "Result: ${name} ${current_version} -> ${latest_version} (${status})"
		return 0
	fi

	# Check if source is from JSC perftools
	local jsc_project jsc_result

	# Temporarily disable exit-on-error for command substitution
	set +e
	jsc_project="$(parse_jsc_perftools_package "${source0}")"
	jsc_result=$?
	set -e

	if [[ ${jsc_result} -eq 0 ]]; then
		debug_info "Checking JSC perftools package: ${jsc_project}"

		# Get latest version
		local latest_version version_result
		# Temporarily disable exit-on-error for command substitution
		set +e
		latest_version="$(get_latest_jsc_perftools_version "${jsc_project}")"
		version_result=$?
		set -e

		if [[ ${version_result} -ne 0 ]]; then
			echo "${name}|${current_version}|ERROR|Failed to fetch JSC version|perftools.pages.jsc.fz-juelich.de/cicd/${jsc_project}" >>"${RESULTS_FILE}"
			return 0
		fi

		# Compare versions
		local comparison
		# Temporarily disable exit-on-error for command substitution
		set +e
		comparison="$(compare_versions "${current_version}" "${latest_version}")"
		set -e

		# Determine status
		local status
		case "${comparison}" in
		"older") status="UPDATE_AVAILABLE" ;;
		"same") status="UP_TO_DATE" ;;
		"newer") status="AHEAD" ;;
		*) status="UNKNOWN" ;;
		esac

		# Write result
		echo "${name}|${current_version}|${latest_version}|${status}|perftools.pages.jsc.fz-juelich.de/cicd/${jsc_project}" >>"${RESULTS_FILE}"

		debug_info "Result: ${name} ${current_version} -> ${latest_version} (${status})"
		return 0
	fi

	# Not a GitHub, GitLab, PyPI, or JSC perftools source
	debug_info "Skipping ${name}: not a GitHub, GitLab, PyPI, or JSC perftools source"
	return 0
}

# Format and display results
display_results() {
	if [[ ! -s "${RESULTS_FILE}" ]]; then
		log_info "No GitHub, GitLab, PyPI, or JSC perftools sourced packages found"
		return 0
	fi

	# Sort results alphabetically by package name
	local sorted_results
	sorted_results="$(mktemp)"
	sort -t'|' -k1,1 "${RESULTS_FILE}" >"${sorted_results}"
	cp "${sorted_results}" "${RESULTS_FILE}"
	rm -f "${sorted_results}"

	case "${OUTPUT_FORMAT}" in
	"table")
		{
			printf "%-35s %-15s %-25s %-20s %s\n" "PACKAGE" "CURRENT" "LATEST" "STATUS" "REPOSITORY"
			printf "%-35s %-15s %-25s %-20s %s\n" "$(printf '%.0s-' {1..35})" "$(printf '%.0s-' {1..15})" "$(printf '%.0s-' {1..25})" "$(printf '%.0s-' {1..20})" "$(printf '%.0s-' {1..50})"

			while IFS='|' read -r name current latest status repo; do
				case "${status}" in
				"UPDATE_AVAILABLE") printf "${YELLOW}%-35s %-15s %-25s %-20s %s${NC}\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				"UP_TO_DATE") printf "${GREEN}%-35s %-15s %-25s %-20s %s${NC}\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				"AHEAD") printf "${BLUE}%-35s %-15s %-25s %-20s %s${NC}\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				"RATE_LIMITED") printf "${YELLOW}%-35s %-15s %-25s %-20s %s${NC}\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				"ERROR") printf "${RED}%-35s %-15s %-25s %-20s %s${NC}\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				*) printf "%-35s %-15s %-25s %-20s %s\n" "${name}" "${current}" "${latest}" "${status}" "${repo}" ;;
				esac
			done <"${RESULTS_FILE}"
		}
		;;
	"json")
		{
			echo "["
			local first=1
			while IFS='|' read -r name current latest status repo; do
				[[ ${first} -eq 0 ]] && echo ","
				jq -n \
					--arg name "${name}" \
					--arg current "${current}" \
					--arg latest "${latest}" \
					--arg status "${status}" \
					--arg repo "${repo}" \
					'{name: ${name}, current_version: ${current}, latest_version: ${latest}, status: ${status}, repository: $repo}'
				first=0
			done <"${RESULTS_FILE}"
			echo "]"
		}
		;;
	"markdown")
		{
			local md_output
			md_output="$(mktemp)"

			{
				echo "# OpenHPC Package Release Status"
				echo
				echo "| Package | Current | Latest | Status | Repository |"
				echo "|---------|---------|--------|--------|------------|"

				while IFS='|' read -r name current latest status repo; do
					# Format status with emojis for better readability
					local status_emoji
					case "${status}" in
					"UPDATE_AVAILABLE") status_emoji="🔄 ${status}" ;;
					"UP_TO_DATE") status_emoji="✅ ${status}" ;;
					"AHEAD") status_emoji="🚀 ${status}" ;;
					"RATE_LIMITED") status_emoji="⏱️ ${status}" ;;
					"ERROR") status_emoji="❌ ${status}" ;;
					*) status_emoji="❓ ${status}" ;;
					esac

					# Generate appropriate URL based on repository type
					local repo_url
					if [[ "${repo}" == gnu.org/* ]]; then
						# GNU project: gnu.org/projectname -> https://ftpmirror.gnu.org/gnu/projectname/
						local project_name="${repo#gnu.org/}"
						repo_url="https://ftpmirror.gnu.org/gnu/${project_name}/"
					elif [[ "${repo}" == pypi.org/project/* ]]; then
						# PyPI: pypi.org/project/packagename -> https://pypi.org/project/packagename/
						repo_url="https://${repo}/"
					elif [[ "${repo}" == perftools.pages.jsc.fz-juelich.de/cicd/* ]]; then
						# JSC perftools: perftools.pages.jsc.fz-juelich.de/cicd/project -> https://...
						repo_url="https://${repo}/"
					elif [[ "${repo}" == */* && "${repo}" != *"/"*"/"* ]]; then
						# GitHub: owner/repo (contains exactly one slash)
						repo_url="https://github.com/${repo}"
					else
						# GitLab or other: hostname/owner/repo (contains multiple slashes or hostname prefix)
						repo_url="https://${repo}"
					fi

					printf "| %-30s | %-15s | %-20s | %-25s | [%s](%s) |\n" \
						"${name}" "${current}" "${latest}" "${status_emoji}" "${repo}" "${repo_url}"
				done <"${RESULTS_FILE}"
			} >"${md_output}"

			# Use glow if available and not disabled
			if [[ ${NO_GLOW} -eq 0 ]] && command -v glow &>/dev/null; then
				glow -w 0 - <"${md_output}"
			else
				cat "${md_output}"
			fi

			rm -f "${md_output}"
		}
		;;
	esac
}

# Generate summary statistics
show_summary() {
	if [[ ! -s "${RESULTS_FILE}" ]]; then
		return 0
	fi

	local total updates up_to_date ahead errors rate_limited
	total="$(wc -l <"${RESULTS_FILE}" | tr -d '\n')"
	updates="$(grep -c "UPDATE_AVAILABLE" "${RESULTS_FILE}" 2>/dev/null || echo 0)"
	up_to_date="$(grep -c "UP_TO_DATE" "${RESULTS_FILE}" 2>/dev/null || echo 0)"
	ahead="$(grep -c "AHEAD" "${RESULTS_FILE}" 2>/dev/null || echo 0)"
	rate_limited="$(grep -c "RATE_LIMITED" "${RESULTS_FILE}" 2>/dev/null || echo 0)"
	errors="$(grep -c "ERROR" "${RESULTS_FILE}" 2>/dev/null || echo 0)"

	# Ensure all values are clean integers - remove any whitespace and newlines
	total=$(echo "${total}" | tr -d ' \n\r\t')
	updates=$(echo "${updates}" | tr -d ' \n\r\t')
	up_to_date=$(echo "${up_to_date}" | tr -d ' \n\r\t')
	ahead=$(echo "${ahead}" | tr -d ' \n\r\t')
	rate_limited=$(echo "${rate_limited}" | tr -d ' \n\r\t')
	errors=$(echo "${errors}" | tr -d ' \n\r\t')

	# Ensure they're valid numbers (default to 0 if empty or invalid)
	[[ "${total}" =~ ^[0-9]+$ ]] || total=0
	[[ "${updates}" =~ ^[0-9]+$ ]] || updates=0
	[[ "${up_to_date}" =~ ^[0-9]+$ ]] || up_to_date=0
	[[ "${ahead}" =~ ^[0-9]+$ ]] || ahead=0
	[[ "${rate_limited}" =~ ^[0-9]+$ ]] || rate_limited=0
	[[ "${errors}" =~ ^[0-9]+$ ]] || errors=0

	echo
	log_info "Summary:"
	echo "  Total packages checked: ${total}"
	echo "  Updates available: ${updates}"
	echo "  Up to date: ${up_to_date}"
	echo "  Ahead of latest: ${ahead}"
	if [[ ${rate_limited} -gt 0 ]]; then
		echo "  Rate limited: ${rate_limited}"
	fi
	echo "  Errors: ${errors}"
}

# Main function
main() {
	# Parse command line arguments using getopt
	local parsed_args
	if ! parsed_args=$(getopt -o vpo:t:h \
		--long verbose,prereleases,output:,token:,no-glow,help \
		--name "${SCRIPT_NAME}" -- "$@"); then
		# getopt has already printed an error message
		usage
		exit 1
	fi

	# Set the parsed arguments
	eval set -- "${parsed_args}"

	# Process the arguments
	while true; do
		case "$1" in
		-v | --verbose)
			VERBOSE=1
			shift
			;;
		-p | --prereleases)
			CHECK_PRERELEASES=1
			shift
			;;
		-o | --output)
			OUTPUT_FORMAT="$2"
			if [[ ! "${OUTPUT_FORMAT}" =~ ^(table|json|markdown)$ ]]; then
				log_error "Invalid output format: ${OUTPUT_FORMAT}"
				usage
				exit 1
			fi
			shift 2
			;;
		-t | --token)
			GITHUB_TOKEN="$2"
			shift 2
			;;
		--no-glow)
			NO_GLOW=1
			shift
			;;
		-h | --help)
			usage
			exit 0
			;;
		--)
			shift
			break
			;;
		*)
			log_error "Internal error in argument parsing"
			exit 1
			;;
		esac
	done

	# Check dependencies and setup
	check_dependencies
	setup_rpm_environment

	log_info "Scanning for OpenHPC packages with GitHub, GitLab, PyPI, and JSC perftools sources..."
	debug_info "Using temporary directory: ${TEMP_DIR}"

	# Find and process all spec files
	local processed=0 rate_limited=0 result_code

	# Use mapfile to read all spec files first
	local spec_files=()
	while IFS= read -r -d '' spec_file; do
		spec_files+=("${spec_file}")
	done < <(find components -name "*.spec" -print0)

	log_info "Found ${#spec_files[@]} spec files to process"

	# Process each spec file
	for spec_file in "${spec_files[@]}"; do
		# Temporarily disable exit-on-error for the entire processing
		set +e
		process_spec_file "${spec_file}"
		result_code=$?
		set -e

		if [[ ${result_code} -eq 2 ]]; then
			# Rate limited - stop processing
			rate_limited=1
			break
		elif [[ ${result_code} -eq 0 ]]; then
			processed=$((processed + 1))
		fi
	done

	if [[ ${rate_limited} -eq 1 ]]; then
		log_warn "Processing stopped due to GitHub API rate limiting"
		log_info "To process all packages, set GITHUB_TOKEN environment variable"
		log_info "Get a token at: https://github.com/settings/tokens"
	fi

	log_info "Processed ${processed} spec files"

	# Display results
	display_results
	show_summary

	# Set exit code based on results
	if [[ -s "${RESULTS_FILE}" ]]; then
		local updates_available
		updates_available="$(grep -c "UPDATE_AVAILABLE" "${RESULTS_FILE}" 2>/dev/null || echo 0)"
		updates_available=$(echo "${updates_available}" | tr -d ' \n\r\t')

		if [[ "${updates_available}" =~ ^[0-9]+$ ]] && [[ "${updates_available}" -gt 0 ]]; then
			exit 1 # Updates are available
		else
			exit 0 # All packages are up to date (or no packages processed)
		fi
	else
		exit 0 # No results file means no packages were processed
	fi
}

# Run main function
main "$@"
