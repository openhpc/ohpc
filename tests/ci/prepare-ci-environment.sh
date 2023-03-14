#!/bin/bash

# shellcheck disable=SC2086

set -x
set -e

FACTORY_VERSION=2.7

if [ ! -e /etc/os-release ]; then
	echo "Cannot detect OS without /etc/os-release"
	exit 1
fi

# shellcheck disable=SC1091
. /etc/os-release

PKG_MANAGER=zypper
COMMON_PKGS="wget python3 jq"
UNAME_M=$(uname -m)

retry_counter=0
max_retries=5

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
		break
	fi
done

if [ "${ID}" = "openEuler" ]; then
	PKG_MANAGER=dnf
	FACTORY_VERSION=3.0
fi

if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		loop_command "${PKG_MANAGER}" -y  install "${COMMON_PKGS}"
		FACTORY_VERSION=3.0
		loop_command wget http://obs.openhpc.community:82/OpenHPC:/"${FACTORY_VERSION}":/Factory/openEuler_22.03/OpenHPC:"${FACTORY_VERSION}":Factory.repo -O /etc/yum.repos.d/ohpc-pre-release.repo
	else
		# We need to figure out if we are running on RHEL (clone) 8 or 9 and
		# rpmdev-vercmp from rpmdevtools is pretty good at comparing versions.
		loop_command "${PKG_MANAGER}" -y  install rpmdevtools crypto-policies-scripts "${COMMON_PKGS}"

		# Exit status is 0 if the EVR's are equal, 11 if EVR1 is newer, and 12 if EVR2
	        # is newer.  Other exit statuses indicate problems.
		set +e
		rpmdev-vercmp 9 "${VERSION_ID}"
		if [ "$?" -eq "11" ]; then
			OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/CentOS_8/${UNAME_M}/ohpc-release-2-1.el8.${UNAME_M}.rpm"
		else
			FACTORY_VERSION=3.0
			# This is our RHEL 9 pre-release repository
			loop_command wget http://obs.openhpc.community:82/OpenHPC:/"${FACTORY_VERSION}":/Factory/EL_9/OpenHPC:"${FACTORY_VERSION}":Factory.repo -O /etc/yum.repos.d/ohpc-pre-release.repo
			# The OBS signing key is too old
			update-crypto-policies --set LEGACY
			NINE=1
		fi
		set -e
	fi
else
	OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/Leap_15/${UNAME_M}/ohpc-release-2-1.leap15.${UNAME_M}.rpm"
fi

if [ "${FACTORY_VERSION}" != "" ]; then
	FACTORY_REPOSITORY=http://obs.openhpc.community:82/OpenHPC:/"${FACTORY_VERSION}":/Factory/
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		if [ -z "${NINE}" ]; then
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_9"
		elif [ "${ID}" = "openEuler" ]; then
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}openEuler_22.03"
		else
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_9"
		fi
		FACTORY_REPOSITORY_DESTINATION="/etc/yum.repos.d/obs.repo"
	else
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}Leap_15"
		FACTORY_REPOSITORY_DESTINATION="/etc/zypp/repos.d/obs.repo"
	fi
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/OpenHPC:${FACTORY_VERSION}:Factory.repo"
fi

dnf_rhel() {
	loop_command "${PKG_MANAGER}" -y install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	loop_command "${PKG_MANAGER}" config-manager --set-enabled crb
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" -y install lmod-ohpc
}

dnf_openeuler() {
	loop_command "${PKG_MANAGER}" -y install ${COMMON_PKGS} git dnf-plugins-core rpm-build gawk
	loop_command "${PKG_MANAGER}" -y install ohpc-filesystem lmod-ohpc hostname
}

if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		dnf_openeuler
	else
		dnf_rhel
	fi
	adduser ohpc
else
	loop_command "${PKG_MANAGER}" -n install ${COMMON_PKGS} awk rpmbuild
	loop_command "${PKG_MANAGER}" -n --no-gpg-checks install "${OHPC_RELEASE}"
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" -n --no-gpg-checks install lmod-ohpc
	useradd -m ohpc
fi
