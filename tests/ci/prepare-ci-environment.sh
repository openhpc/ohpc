#!/bin/bash

# shellcheck disable=SC2086

set -e

# Send the xtrace output and every command's stderr down stdout. GitHub Actions
# reads stdout and stderr as separate streams and interleaves them by arrival,
# so without this the "set -x" lines land in whatever log group happens to be
# open when they show up rather than the one they belong to.
exec 2>&1

usage() {
	echo "Usage: $0 [options] <intel>"
	echo
	echo "Options:"
	echo "  --pre-release  Disable setting OHPC_RELEASE"
	echo "  --help         Display this help message"
	echo
	echo "Parameters:"
	echo " intel           Enable Intel OneAPI toolkit"
	echo
	echo "Environment:"
	echo " MODULES_PACKAGE Module tool package to install (default: lmod-ohpc)"
}

FACTORY_VERSION=4.2
MODULES_PACKAGE="${MODULES_PACKAGE:-lmod-ohpc}"
ENABLE_ONEAPI=""
PRE_RELEASE=""
# Set once the environment report is done; see group_start() below.
GROUP_XTRACE=""

while getopts ":-:" opt; do
	case ${opt} in
	-)
		case "${OPTARG}" in
		pre-release)
			PRE_RELEASE="yes"
			;;
		help)
			usage
			exit 0
			;;
		*)
			echo "Invalid option: --${OPTARG}" >&2
			usage
			exit 1
			;;
		esac
		;;
	\?)
		echo "Invalid option: -${OPTARG}" >&2
		usage
		exit 1
		;;
	esac
done
shift $((OPTIND - 1))

if [ $# -eq 1 ]; then
	if [ "${1}" = "intel" ]; then
		ENABLE_ONEAPI="intel-oneapi-toolkit-release-ohpc"
	fi
fi

if [ ! -e /etc/os-release ]; then
	echo "Cannot detect OS without /etc/os-release"
	exit 1
fi

# shellcheck disable=SC1091
. /etc/os-release

PKG_MANAGER=zypper
COMMON_PKGS="wget python3 jq man createrepo_c"
UNAME_M=$(uname -m)
YES="-n"

# Detect if running on RHEL
EPEL_RELEASE="epel-release"
CRB_REPO="crb"
if [ "${ID}" = "rhel" ]; then
	EPEL_RELEASE="https://dl.fedoraproject.org/pub/epel/epel-release-latest-10.noarch.rpm"
	CRB_REPO="codeready-builder-for-ubi-10-${UNAME_M}-rpms"
fi

OBS_SERVER="http://obs.openhpc.community:82/"
PROJECT="OpenHPC4:"
REPOSITORY_URL="http://repos.openhpc.community/OpenHPC/4/"

retry_counter=0
max_retries=5

print_header() {
	echo "############### $1 ###############"
}

# GitHub Actions renders "::group::"/"::endgroup::" as a section of the job log
# that is collapsed by default. Other CI systems just print the plain header.
#
# The helpers also own xtrace: it is switched on inside a group and off again
# before the group closes, so that the trace of a command is collapsed together
# with its output and nothing is traced between two groups. Tracing only starts
# once GROUP_XTRACE is set, which keeps the environment report below readable.
group_start() {
	{ set +x; } 2>/dev/null
	if [ -n "${GITHUB_ACTIONS:-}" ]; then
		echo "::group::$1"
	else
		print_header "$1"
	fi
	if [ -n "${GROUP_XTRACE}" ]; then
		set -x
	fi
}

group_end() {
	{ set +x; } 2>/dev/null
	if [ -n "${GITHUB_ACTIONS:-}" ]; then
		echo "::endgroup::"
	fi
}

print_env() {
	group_start "Install procps"
	loop_command "${PKG_MANAGER}" "${YES}" install procps
	group_end
	# As this script can run on multiple different CI systems
	# the following lines should give some context to the
	# evnvironment of this CI run.
	group_start "Environment variables"
	printenv
	group_end
	# printenv only reports exported variables, so the settings this script
	# derives for itself have to be listed explicitly.
	group_start "Script variables"
	for var in \
		ID ID_LIKE VERSION_ID PLATFORM_ID UNAME_M \
		PKG_MANAGER YES COMMON_PKGS \
		MODULES_PACKAGE ENABLE_ONEAPI PRE_RELEASE \
		EPEL_RELEASE CRB_REPO \
		OBS_SERVER PROJECT REPOSITORY_URL \
		FACTORY_VERSION FACTORY_REPOSITORY FACTORY_REPOSITORY_DESTINATION \
		OHPC_RELEASE; do
		echo "${var}=${!var-}"
	done
	group_end
	group_start "uname -a"
	uname -a || :
	group_end
	group_start "Mounted file systems"
	cat /proc/self/mountinfo || :
	group_end
	group_start "Kernel command line"
	cat /proc/cmdline || :
	group_end
	group_start "ulimit -a"
	ulimit -a
	group_end
	group_start "Available memory"
	free -h
	group_end
	group_start "Available disk space"
	df -h
	group_end
	group_start "Available CPUs"
	lscpu || :
	group_end
}

loop_command() {
	local retry_counter=0
	local max_retries=5

	while true; do
		((retry_counter += 1))
		if [ "${retry_counter}" -gt "${max_retries}" ]; then
			exit 1
		fi
		# shellcheck disable=SC2068
		$@ && break

		# In case it is a network error let's wait a bit.
		echo "Retrying attempt ${retry_counter}"
		sleep "${retry_counter}"
	done
}

for like in ${ID_LIKE}; do
	if [ "${like}" = "fedora" ]; then
		PKG_MANAGER=dnf
		YES="-y"
		break
	fi
done

if [ "${ID}" = "openEuler" ]; then
	PKG_MANAGER=dnf
	YES="-y"
fi

group_start "Install common packages"
loop_command "${PKG_MANAGER}" "${YES}" install "${COMMON_PKGS}"
group_end
if [ "${PRE_RELEASE}" != "yes" ]; then
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		# If there is a release, this is the place to install the corresponding
		# release RPM. As long as there is no release RPM only the OBS repository
		# is used directly.
		if [ "${ID}" = "openEuler" ]; then
			OHPC_RELEASE="${REPOSITORY_URL}openEuler_24.03/${UNAME_M}/ohpc-release-4-1.oe2403.${UNAME_M}.rpm"
		else
			OHPC_RELEASE="${REPOSITORY_URL}EL_10/${UNAME_M}/ohpc-release-4-1.el10.${UNAME_M}.rpm"
		fi
	else
		OHPC_RELEASE="${REPOSITORY_URL}Leap_15/${UNAME_M}/ohpc-release-4-1.leap15.${UNAME_M}.rpm"
	fi
fi

if [ "${FACTORY_VERSION}" != "" ]; then
	FACTORY_REPOSITORY="${OBS_SERVER}""${PROJECT}"/"${FACTORY_VERSION}":/Factory/
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		if [ "${ID}" = "openEuler" ]; then
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}openEuler_24.03"
		else
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_10"
		fi
		FACTORY_REPOSITORY_DESTINATION="/etc/yum.repos.d/obs.repo"
	else
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}Leap_15"
		FACTORY_REPOSITORY_DESTINATION="/etc/zypp/repos.d/obs.repo"
	fi
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/${PROJECT}${FACTORY_VERSION}:Factory.repo"
fi

dnf_rhel() {
	# Enable keepcache for dnf if OneAPI is enabled to cache large Intel packages
	if [ -n "${ENABLE_ONEAPI}" ]; then
		echo "keepcache=1" >>/etc/dnf/dnf.conf
	fi
	group_start "Install build dependencies"
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} lua "${EPEL_RELEASE}" dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	group_end
	group_start "Enable the ${CRB_REPO} repository"
	loop_command "${PKG_MANAGER}" config-manager --set-enabled "${CRB_REPO}"
	group_end
	if [ "${FACTORY_VERSION}" != "" ]; then
		group_start "Add the OBS Factory repository"
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
		group_end
	fi
	group_start "Install ${MODULES_PACKAGE}, bats and ccache"
	loop_command "${PKG_MANAGER}" "${YES}" install "${MODULES_PACKAGE}" bats ccache "${ENABLE_ONEAPI}"
	group_end
}

dnf_openeuler() {
	group_start "Install build dependencies"
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} git dnf-plugins-core rpm-build gawk "${OHPC_RELEASE}"
	group_end
	if [ "${FACTORY_VERSION}" != "" ]; then
		group_start "Add the OBS Factory repository"
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
		group_end
	fi
	group_start "Add the openEuler OpenHPC repository"
	loop_command wget -P /etc/yum.repos.d/ https://eur.openeuler.openatom.cn/coprs/openhpc/OpenHPC/repo/openeuler-24.03_LTS_SP2/openhpc-OpenHPC-openeuler-24.03_LTS_SP2.repo
	group_end
	group_start "Install ${MODULES_PACKAGE}, bats and ccache"
	loop_command "${PKG_MANAGER}" "${YES}" install ohpc-filesystem "${MODULES_PACKAGE}" hostname bats ccache
	group_end

	# openEuler's ccache.csh lacks a final newline: tcsh then fails to find
	# its last "endif" when skipping the block it closes, which happens in
	# every root login shell, and aborts /etc/csh.login before the module
	# startup script is sourced ("then: then/endif not found")
	if [ -n "$(tail -c1 /etc/profile.d/ccache.csh 2>/dev/null)" ]; then
		echo >>/etc/profile.d/ccache.csh
	fi

	# We need to have the latest glibc installed for valgrind tests on openEuler
	group_start "Upgrade all packages"
	"${PKG_MANAGER}" "${YES}" upgrade
	group_end
}

print_env

# From here on the interesting part is what gets installed, so let the groups
# trace their own commands.
GROUP_XTRACE=1
if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		dnf_openeuler
	else
		dnf_rhel
	fi
	group_start "Create the ohpc user"
	adduser ohpc || true
	group_end
else
	group_start "Install build dependencies"
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install ${COMMON_PKGS} awk rpmbuild bats "${OHPC_RELEASE}"
	group_end
	if [ "${FACTORY_VERSION}" != "" ]; then
		group_start "Add the OBS Factory repository"
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
		group_end
	fi
	group_start "Install ${MODULES_PACKAGE}"
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install "${MODULES_PACKAGE}" "${ENABLE_ONEAPI}"
	group_end
	group_start "Create the ohpc user"
	useradd -m ohpc || true
	group_end
fi

# Setup ccache
group_start "Set up ccache"
echo "cache_dir=/var/cache/ccache" >/etc/ccache.conf
install -d -o ohpc -g ohpc /var/cache/ccache
group_end
