#!/bin/bash

# shellcheck disable=SC2086

set -x
set -e

FACTORY_VERSION=2.10
ENABLE_ONEAPI=""
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

retry_counter=0
max_retries=5

print_header() {
	echo "############### $1 ###############"
}

print_env() {
	set +x
	loop_command "${PKG_MANAGER}" "${YES}" install procps
	# As this script can run on multiple different CI systems
	# the following lines should give some context to the
	# evnvironment of this CI run.
	print_header "Environment variables"
	printenv
	print_header "uname -a"
	uname -a || :
	print_header "Mounted file systems"
	cat /proc/self/mountinfo || :
	print_header "Kernel command line"
	cat /proc/cmdline || :
	print_header "ulimit -a"
	ulimit -a
	print_header "Available memory"
	free -h
	print_header "Available disk space"
	df -h
	print_header "Available CPUs"
	lscpu || :
	set -x
}

loop_command() {
	local retry_counter=0
	local max_retries=5

	while true; do
		(( retry_counter+=1 ))
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

if [ "${PKG_MANAGER}" = "dnf" ]; then
	# We need to figure out if we are running on RHEL (clone) 8 or 9 and
	# rpmdev-vercmp from rpmdevtools is pretty good at comparing versions.
	loop_command "${PKG_MANAGER}" "${YES}"  install rpmdevtools crypto-policies-scripts "${COMMON_PKGS}"

	# Exit status is 0 if the EVR's are equal, 11 if EVR1 is newer, and 12 if EVR2
		# is newer.  Other exit statuses indicate problems.
	set +e
	rpmdev-vercmp 9 "${VERSION_ID}"
	if [ "$?" -eq "11" ]; then
		OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/CentOS_8/${UNAME_M}/ohpc-release-2-1.el8.${UNAME_M}.rpm"
	fi
	set -e
else
	OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/Leap_15/${UNAME_M}/ohpc-release-2-1.leap15.${UNAME_M}.rpm"
fi

if [ "${FACTORY_VERSION}" != "" ]; then
	FACTORY_REPOSITORY=http://obs.openhpc.community:82/OpenHPC:/"${FACTORY_VERSION}":/Factory/
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		if [ -z "${NINE}" ]; then
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_8"
		fi
		FACTORY_REPOSITORY_DESTINATION="/etc/yum.repos.d/obs.repo"
	else
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}Leap_15"
		FACTORY_REPOSITORY_DESTINATION="/etc/zypp/repos.d/obs.repo"
	fi
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/OpenHPC:${FACTORY_VERSION}:Factory.repo"
fi

dnf_rhel() {
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	if [ -z "${NINE}" ]; then
		loop_command "${PKG_MANAGER}" config-manager --set-enabled powertools
		if "${PKG_MANAGER}" repolist --all | grep -q "^devel"; then
			loop_command "${PKG_MANAGER}" config-manager --set-enabled devel
		fi
	else
		loop_command "${PKG_MANAGER}" config-manager --set-enabled crb
	fi
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" install lmod-ohpc ccache "${ENABLE_ONEAPI}"
}

dnf_openeuler() {
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} git dnf-plugins-core rpm-build gawk
	loop_command "${PKG_MANAGER}" "${YES}" install ohpc-filesystem lmod-ohpc hostname ccache
}

print_env

if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		dnf_openeuler
	else
		dnf_rhel
	fi
	adduser ohpc || true
else
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} awk rpmbuild ccache
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install "${OHPC_RELEASE}"
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks refresh
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install lmod-ohpc "${ENABLE_ONEAPI}"
	groupadd ohpc || true
	useradd -m ohpc -g ohpc || true
fi

# Setup ccache
echo "cache_dir=/var/cache/ccache" >/etc/ccache.conf
install -d -o ohpc -g ohpc /var/cache/ccache
