#!/bin/bash
#
#----------------------------------------------------------------------
# Simple utility to query OpenHPC repo to determine number of packages
# available.
#----------------------------------------------------------------------

show_usage() {
	echo "usage: $(basename "$0") [-f] [-d|--debug] [--no-glow] [version]"
	echo "  -f         Query factory repositories (auto-detects newest or uses specified version)"
	echo "  -d|--debug Enable additional output (validation messages, progress info, etc.)"
	echo "  --no-glow  Disable piping through glow for markdown formatting"
	echo "  -h         Show this help message"
	echo ""
	echo "version can be:"
	echo "  major         (e.g., '4') - finds newest 4.x version"
	echo "  major.minor   (e.g., '4.1') - uses specific version"
	echo "  major.minor.micro (e.g., '4.1.0') - uses specific version"
	echo "  (empty)       - auto-detects from git branch"
	echo ""
	echo "By default, only the markdown table is output. Use -d|--debug for additional messages."
}

# Debug output function - only prints if DEBUG=1
debug() {
	if [[ ${DEBUG} -eq 1 ]]; then
		echo "$@" >&2
	fi
}

# Version parameter handling moved after getopts parsing

command -v repoquery >&/dev/null || {
	echo "repoquery must be installed locally"
	exit 1
}

command -v curl >&/dev/null || {
	echo "curl must be installed locally"
	exit 1
}

command -v rpmdev-vercmp >&/dev/null || {
	echo "rpmdev-vercmp must be installed locally"
	exit 1
}

# Parse command line arguments
USE_FACTORY=0
NO_GLOW=0
DEBUG=0

# Use getopt to handle long options
if ! PARSED_OPTS=$(getopt -o fhd --long no-glow,help,debug -n "$(basename "$0")" -- "$@"); then
	show_usage
	exit 1
fi

# Set the parsed options
eval set -- "${PARSED_OPTS}"

while true; do
	case "$1" in
	-f)
		USE_FACTORY=1
		shift
		;;
	-d | --debug)
		DEBUG=1
		shift
		;;
	-h | --help)
		show_usage
		exit 0
		;;
	--no-glow)
		NO_GLOW=1
		shift
		;;
	--)
		shift
		break
		;;
	*)
		echo "Internal error parsing options" >&2
		exit 1
		;;
	esac
done

# Handle version parameter (optional for factory mode)
if [[ $# -eq 0 ]]; then
	# Auto-detect from git branch
	if git rev-parse --git-dir >/dev/null 2>&1; then
		current_branch=$(git branch --show-current 2>/dev/null || git rev-parse --abbrev-ref HEAD 2>/dev/null)
		if [[ ${current_branch} =~ ^([0-9]+)\.x$ ]]; then
			version="${BASH_REMATCH[1]}"
			debug "Auto-detected version ${version} from git branch ${current_branch}"
		else
			echo "Error: Could not auto-detect version from git branch '${current_branch}'"
			echo "Please specify a version explicitly"
			exit 1
		fi
	else
		echo "Error: Not in a git repository and no version specified"
		exit 1
	fi
elif [[ $# -eq 1 ]]; then
	version="$1"
else
	echo "Error: too many arguments"
	show_usage
	exit 1
fi
arches="x86_64 aarch64 noarch"

# Parse version components efficiently in one go
parse_version() {
	local ver="$1"
	IFS='.' read -r major_ver minor_dig micro_ver <<<"${ver}"
	minor_ver="${major_ver}.${minor_dig:-0}"
	micro_ver="${micro_ver:-0}"
}

parse_version "${version}"

# Set OS list based on major version
if [[ ${major_ver} -eq 4 ]]; then
	oses="EL_10 openEuler_24.03"
else
	oses="EL_9 Leap_15 openEuler_22.03"
fi

# Cache for major versions to avoid redundant curl calls
declare -A major_version_cache

# Function to get all available major versions from OBS/staging
get_all_major_versions() {
	local repo_type="$1" # "factory" or "staging"

	# Check cache first
	if [[ -n ${major_version_cache[${repo_type}]} ]]; then
		echo "${major_version_cache[${repo_type}]}"
		return 0
	fi

	local base_url
	local pattern
	local result

	if [[ ${repo_type} == "factory" ]]; then
		base_url="http://obs.openhpc.community:82/"
		pattern="^OpenHPC[0-9]"
		result=$(curl -s "${base_url}" |
			grep -o 'href="[^"]*"' |
			sed 's/href="//;s/"//' |
			grep "${pattern}" |
			sed 's/^OpenHPC//;s/:$//' |
			sort -V)
	else
		base_url="http://repos.openhpc.community/.staging/OpenHPC"
		pattern="^[0-9]"
		result=$(curl -s "${base_url}/" |
			grep -o 'href="[^"]*"' |
			sed 's/href="//;s/"//' |
			grep "${pattern}" |
			sed 's/\/$//' |
			sort -V)
	fi

	# Cache the result
	major_version_cache[${repo_type}]="${result}"
	echo "${result}"
}

# Function to scan OBS directory listing and find available versions
get_obs_versions() {
	local major_version=$1
	local obs_base="http://obs.openhpc.community:82/OpenHPC${major_version}:"

	debug "Scanning OBS directory listing: ${obs_base}"

	# Get directory listing and extract version directories
	curl -s "${obs_base}/" |
		grep -o 'href="[^"]*"' |
		sed 's/href="//;s/"//' |
		sed 's/^\.*\///' |
		sed 's/[:/]*$//' |
		grep "^${major_version}\.[0-9]" |
		grep -v "\.x$" |
		sort -V
}

# Function to scan staging directory listing and find available versions
get_staging_versions() {
	local major_version=$1
	local staging_base="http://repos.openhpc.community/.staging/OpenHPC/${major_version}"

	debug "Scanning staging directory listing: ${staging_base}"

	# Get base version if it exists
	local versions=""
	if curl -s --head "${staging_base}/" | head -n 1 | grep -q "200 OK"; then
		versions="${major_version}.0"
	fi

	# Get update versions
	local update_versions
	update_versions=$(curl -s "${staging_base}/" |
		grep -o 'href="[^"]*"' |
		sed 's/href="//;s/"//' |
		grep "^update\\.${major_version}\\." |
		sed "s/^update\\.//" |
		sed 's/\/$//' |
		sort -V)

	# Combine base and update versions
	if [[ -n ${update_versions} ]]; then
		if [[ -n ${versions} ]]; then
			versions="${versions} ${update_versions}"
		else
			versions="${update_versions}"
		fi
	fi

	echo "${versions}"
}

# Function to find the newest version using rpmdev-vercmp
find_newest_version() {
	local versions="$1"
	local newest=""

	for version in ${versions}; do
		if [[ -z ${newest} ]]; then
			newest="${version}"
		else
			# Use rpmdev-vercmp to compare versions (0 = equal, 11 = first > second, 12 = first < second)
			rpmdev-vercmp "${version}" "${newest}" >/dev/null 2>&1
			exit_code=$?
			if [[ ${exit_code} -eq 11 ]]; then
				newest="${version}"
			fi
		fi
	done

	echo "${newest}"
}

if [[ ${USE_FACTORY} -eq 1 ]]; then
	# Determine what version(s) to query
	IFS='.' read -ra version_parts <<<"${version}"
	num_parts=${#version_parts[@]}

	if [[ ${num_parts} -eq 1 ]]; then
		# Only major version specified - find newest version in OBS
		debug "Scanning for newest ${major_ver}.x version in factory repositories..."
		available_versions=$(get_obs_versions "${major_ver}")
		if [[ -z ${available_versions} ]]; then
			echo "Error: No versions found for major version ${major_ver} in factory repositories" >&2
			echo "OBS URL tested: http://obs.openhpc.community:82/OpenHPC${major_ver}:/" >&2
			echo "" >&2
			echo "Available OpenHPC major versions in factory:" >&2

			# Query for all available major versions in OBS
			all_factory_versions=$(get_all_major_versions "factory")

			if [[ -n ${all_factory_versions} ]]; then
				for factory_major_ver in ${all_factory_versions}; do
					echo "  OpenHPC ${factory_major_ver}.x (factory)" >&2
				done
			else
				echo "  No factory versions found" >&2
			fi

			echo "" >&2
			echo "Please use one of the available major versions or try non-factory mode instead" >&2
			exit 1
		fi
		target_version=$(find_newest_version "${available_versions}")
		debug "Found newest version: ${target_version}"
		releases="${target_version}"
	else
		# Specific version requested - validate it exists
		debug "Validating version ${version} exists in factory repositories..."
		available_versions=$(get_obs_versions "${major_ver}")

		# Check if the requested version exists in the available versions
		version_found=0
		for available_version in ${available_versions}; do
			if [[ "${available_version}" == "${version}" ]]; then
				version_found=1
				break
			fi
		done

		if [[ ${version_found} -eq 0 ]]; then
			echo "Error: Version ${version} not found in factory repositories" >&2
			echo "OBS URL tested: http://obs.openhpc.community:82/OpenHPC${major_ver}:/" >&2
			echo "" >&2

			if [[ -n ${available_versions} ]]; then
				echo "Available versions for OpenHPC ${major_ver}.x in factory:" >&2
				for ver in ${available_versions}; do
					echo "  ${ver}" >&2
				done
			else
				echo "No versions found for OpenHPC ${major_ver}.x in factory repositories" >&2
				echo "" >&2
				echo "Available OpenHPC major versions in factory:" >&2

				# Query for all available major versions in OBS
				all_factory_versions=$(get_all_major_versions "factory")

				if [[ -n ${all_factory_versions} ]]; then
					for factory_major_ver in ${all_factory_versions}; do
						echo "  OpenHPC ${factory_major_ver}.x (factory)" >&2
					done
				fi
			fi

			echo "" >&2
			echo "Please use one of the available versions or try non-factory mode instead" >&2
			exit 1
		fi

		debug "Version ${version} validated successfully"
		releases="${version}"
	fi
else
	# Non-factory mode - determine what version(s) to query
	IFS='.' read -ra version_parts <<<"${version}"
	num_parts=${#version_parts[@]}

	if [[ ${num_parts} -eq 1 ]]; then
		# Only major version specified - find newest version in staging
		debug "Scanning for newest ${major_ver}.x version in staging repositories..."
		available_versions=$(get_staging_versions "${major_ver}")
		if [[ -z ${available_versions} ]]; then
			echo "Error: No versions found for major version ${major_ver} in staging repositories" >&2
			echo "Staging URL tested: http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/" >&2
			echo "" >&2
			echo "Available OpenHPC major versions in staging:" >&2

			# Query for all available major versions
			all_major_versions=$(get_all_major_versions "staging")

			if [[ -n ${all_major_versions} ]]; then
				for staging_major_ver in ${all_major_versions}; do
					echo "  OpenHPC ${staging_major_ver}.x (staging)" >&2
				done
			else
				echo "  No staging versions found" >&2
			fi

			echo "" >&2
			echo "Please use one of the available major versions or try factory mode (-f) instead" >&2
			exit 1
		fi
		target_version=$(find_newest_version "${available_versions}")
		debug "Found newest version: ${target_version}"

		# Set the version variables for the newest found version
		version="${target_version}"
		parse_version "${target_version}"
	elif [[ ${minor_dig} -gt 0 ]]; then
		# Specific minor version requested - validate it exists
		debug "Validating version ${minor_ver} exists in staging repositories..."
		test_url="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/update.${minor_ver}"

		if ! curl -s --head "${test_url}/" | head -n 1 | grep -q "200 OK"; then
			echo "Error: Version ${minor_ver} not found in staging repositories" >&2
			echo "URL tested: ${test_url}/" >&2
			echo "" >&2
			echo "Available versions:" >&2

			# Get all available versions for this major version
			available_versions=$(get_staging_versions "${major_ver}")
			if [[ -n ${available_versions} ]]; then
				echo "  Available versions for OpenHPC ${major_ver}.x:" >&2
				for ver in ${available_versions}; do
					echo "    ${ver}" >&2
				done
			else
				echo "  No versions found for OpenHPC ${major_ver}.x" >&2
			fi

			# Query for all available major versions
			echo "" >&2
			echo "All available OpenHPC major versions:" >&2
			all_versions=$(get_all_major_versions "staging")

			for major_ver_available in ${all_versions}; do
				echo "  OpenHPC ${major_ver_available}.0" >&2
			done

			echo "" >&2
			echo "Please use one of the available versions or try factory mode (-f) instead" >&2
			exit 1
		fi
		debug "Version ${minor_ver} validated successfully"
	else
		# For major-only versions with .0 (like "4.0"), validate the base version exists
		debug "Validating base version ${major_ver}.0 exists in staging repositories..."
		base_version_url="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}"

		if ! curl -s --head "${base_version_url}/" | head -n 1 | grep -q "200 OK"; then
			echo "Error: Base version ${major_ver}.0 not found in staging repositories" >&2
			echo "URL tested: ${base_version_url}/" >&2
			echo "" >&2

			# Get all available versions for this major version if any exist
			available_versions=$(get_staging_versions "${major_ver}" 2>/dev/null || echo "")
			if [[ -n ${available_versions} ]]; then
				echo "Available versions for OpenHPC ${major_ver}.x:" >&2
				for ver in ${available_versions}; do
					echo "  ${ver}" >&2
				done
				echo "" >&2
			fi

			echo "Available OpenHPC major versions:" >&2

			# Query for all available major versions
			all_versions=$(get_all_major_versions "staging")

			for major_ver_available in ${all_versions}; do
				echo "  OpenHPC ${major_ver_available}.0" >&2
			done

			echo "" >&2
			echo "Please use one of the available major versions or try factory mode (-f) instead" >&2
			exit 1
		fi
		debug "Base version ${major_ver}.0 validated successfully"
	fi

	debug "Querying available package counts: minor_ver=${minor_ver}, micro_ver=${micro_ver}"
fi

total=0

# Initialize arrays for markdown output
declare -A os_totals
declare -A arch_totals
arch_totals["x86_64"]=0
arch_totals["aarch64"]=0
arch_totals["noarch"]=0
arch_totals["total"]=0

if [[ ${USE_FACTORY} -eq 1 ]]; then
	debug "[Querying from Factory repositories for all releases in ${major_ver}.x]"

	# Create clean releases list for markdown output using array
	clean_releases_array=()

	# Process each release found
	for release in ${releases}; do
		# The release should already be cleaned by get_obs_versions
		clean_release="${release}"

		# Add to clean releases array
		clean_releases_array+=("${clean_release}")

		# Parse version components efficiently
		IFS='.' read -r release_major_ver release_minor_dig release_micro_ver <<<"${clean_release}"

		# Set defaults for missing components
		release_minor_dig="${release_minor_dig:-0}"
		release_micro_ver="${release_micro_ver:-0}"
		release_minor_ver="${release_major_ver}.${release_minor_dig}"

		# Debug output
		debug "Debug: original release='${release}', clean_release='${clean_release}', minor_ver='${release_minor_ver}', micro_ver='${release_micro_ver}'"

		for os in ${oses}; do
			# Construct factory repository URL: OpenHPC4:/4.0:/Factory/EL_10/ or OpenHPC4:/4.0.1:/Factory/EL_10/
			if [[ -z ${release_micro_ver} || ${release_micro_ver} == "0" ]]; then
				repobase="http://obs.openhpc.community:82/OpenHPC${major_ver}:/${release_minor_ver}:/Factory/${os}/"
			else
				repobase="http://obs.openhpc.community:82/OpenHPC${major_ver}:/${clean_release}:/Factory/${os}/"
			fi

			# Initialize release/OS totals for markdown
			os_totals["${clean_release}:${os}:x86_64"]=0
			os_totals["${clean_release}:${os}:aarch64"]=0
			os_totals["${clean_release}:${os}:noarch"]=0
			os_totals["${clean_release}:${os}:total"]=0

			for arch in ${arches}; do
				# Factory repos only have base repository, no update repo
				numrpms=$(repoquery --archlist="${arch}" --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' 2>/dev/null | wc -l)

				((total = total + numrpms))

				# Store data for markdown output (using release:os:arch format for factory mode)
				os_totals["${clean_release}:${os}:${arch}"]=${numrpms}
				((os_totals["${clean_release}:${os}:total"] += numrpms))
				((arch_totals["${arch}"] += numrpms))
				((arch_totals["total"] += numrpms))
			done
		done
	done

else
	# Non-factory mode (staging repositories)
	for os in ${oses}; do
		repobase="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/${os}"

		if [[ ${minor_dig} -gt 0 ]]; then
			repoupdate="http://repos.openhpc.community/.staging/OpenHPC/${major_ver}/update.${minor_ver}/${os}"
		fi

		# Initialize OS totals for markdown
		os_totals["${os}:x86_64"]=0
		os_totals["${os}:aarch64"]=0
		os_totals["${os}:noarch"]=0
		os_totals["${os}:total"]=0

		for arch in ${arches}; do
			if [[ ${minor_dig} -eq 0 ]]; then
				numrpms=$(repoquery --archlist="${arch}" --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' 2>/dev/null | wc -l)
			else
				numrpms=$(repoquery --archlist="${arch}" --repofrompath="ohpc-base,${repobase}" --repoid=ohpc-base '*' \
					--repofrompath="ohpc-update,${repoupdate}" --repoid=ohpc-update '*' 2>/dev/null | wc -l)
			fi

			((total = total + numrpms))

			# Store data for markdown output
			os_totals["${os}:${arch}"]=${numrpms}
			((os_totals["${os}:total"] += numrpms))
			((arch_totals["${arch}"] += numrpms))
			((arch_totals["total"] += numrpms))
		done
	done
fi

# Function to generate markdown table
generate_markdown_table() {
	if [[ ${USE_FACTORY} -eq 1 ]]; then
		printf "| %-14s | %-19s | %11s | %10s | %10s | %9s |\n" "Release" "Base OS" "aarch64" "x86_64" "noarch" "Total"
		printf "|%s|%s|%s|%s|%s|%s|\n" "----------------" "---------------------" "-------------" "------------" "------------" "-----------"

		for release in "${clean_releases_array[@]}"; do
			for os in ${oses}; do
				printf "| %-14s | %-19s | %11s | %10s | %10s | %9s |\n" \
					"${release}" "${os}" \
					"${os_totals["${release}:${os}:aarch64"]}" \
					"${os_totals["${release}:${os}:x86_64"]}" \
					"${os_totals["${release}:${os}:noarch"]}" \
					"${os_totals["${release}:${os}:total"]}"
			done
		done

		printf "| %-14s | %-19s | %11s | %10s | %10s | %9s |\n" \
			"**Total**" "**All**" \
			"**${arch_totals["aarch64"]}**" \
			"**${arch_totals["x86_64"]}**" \
			"**${arch_totals["noarch"]}**" \
			"**${arch_totals["total"]}**"
	else
		printf "| %-19s | %11s | %10s | %10s | %9s |\n" "Base OS" "aarch64" "x86_64" "noarch" "Total"
		printf "|%s|%s|%s|%s|%s|\n" "---------------------" "-------------" "------------" "------------" "-----------"

		for os in ${oses}; do
			printf "| %-19s | %11s | %10s | %10s | %9s |\n" \
				"${os}" \
				"${os_totals["${os}:aarch64"]}" \
				"${os_totals["${os}:x86_64"]}" \
				"${os_totals["${os}:noarch"]}" \
				"${os_totals["${os}:total"]}"
		done

		printf "| %-19s | %11s | %10s | %10s | %9s |\n" \
			"**Total**" \
			"**${arch_totals["aarch64"]}**" \
			"**${arch_totals["x86_64"]}**" \
			"**${arch_totals["noarch"]}**" \
			"**${arch_totals["total"]}**"
	fi
}

# Check if glow is available and pipe through it for nice formatting
if [[ ${NO_GLOW} -eq 0 ]] && command -v glow >&/dev/null; then
	generate_markdown_table | glow -
else
	generate_markdown_table
fi
