#!/bin/bash

# shellcheck disable=SC2086

set -x
set -e

FACTORY_VERSION=3.0

if [ ! -e /etc/os-release ]; then
	echo "Cannot detect OS without /etc/os-release"
	exit 1
fi

# shellcheck disable=SC1091
. /etc/os-release

PKG_MANAGER=zypper
COMMON_PKGS="wget python3 jq"
YES="-n"

OBS_SERVER="http://obs.openhpc.community:82/"
PROJECT="OpenHPC3:"

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
		YES="-y"
		break
	fi
done

if [ "${ID}" = "openEuler" ]; then
	PKG_MANAGER=dnf
	YES="-y"
fi

loop_command "${PKG_MANAGER}" "${YES}" install "${COMMON_PKGS}"
if [ "${PKG_MANAGER}" = "dnf" ]; then
	# If there is a release, this is the place to install the corresponding
	# release RPM. As long as there is no release RPM only the OBS repository
	# is used directly.
	if [ "${ID}" = "openEuler" ]; then
		loop_command wget "${OBS_SERVER}""${PROJECT}"/"${FACTORY_VERSION}":/Factory/openEuler_22.03/OpenHPC3:"${FACTORY_VERSION}":Factory.repo -O /etc/yum.repos.d/ohpc-pre-release.repo
	else
		loop_command wget "${OBS_SERVER}""${PROJECT}"/"${FACTORY_VERSION}":/Factory/EL_9/OpenHPC3:"${FACTORY_VERSION}":Factory.repo -O /etc/yum.repos.d/ohpc-pre-release.repo
	fi
else
	loop_command wget "${OBS_SERVER}""${PROJECT}"/"${FACTORY_VERSION}":/Factory/Leap_15/OpenHPC3:"${FACTORY_VERSION}":Factory.repo -O /etc/zypp/repos.d/ohpc-pre-release.repo
fi

if [ "${FACTORY_VERSION}" != "" ]; then
	FACTORY_REPOSITORY="${OBS_SERVER}""${PROJECT}"/"${FACTORY_VERSION}":/Factory/
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		if [ "${ID}" = "openEuler" ]; then
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}openEuler_22.03"
		else
			FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_9"
		fi
		FACTORY_REPOSITORY_DESTINATION="/etc/yum.repos.d/obs.repo"
	else
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}Leap_15"
		FACTORY_REPOSITORY_DESTINATION="/etc/zypp/repos.d/obs.repo"
	fi
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/OpenHPC3:${FACTORY_VERSION}:Factory.repo"
fi

dnf_rhel() {
	loop_command "${PKG_MANAGER}" -y install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	if [ -z "${NINE}" ]; then
		loop_command "${PKG_MANAGER}" config-manager --set-enabled powertools
		loop_command "${PKG_MANAGER}" config-manager --set-enabled devel
	else
		loop_command "${PKG_MANAGER}" -y install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
		if [ -z "${NINE}" ]; then
			loop_command "${PKG_MANAGER}" config-manager --set-enabled powertools
			loop_command "${PKG_MANAGER}" config-manager --set-enabled devel
		else
			loop_command "${PKG_MANAGER}" config-manager --set-enabled crb
		fi
	fi
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" install lmod-ohpc
}

dnf_openeuler() {
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} git dnf-plugins-core rpm-build gawk
	loop_command "${PKG_MANAGER}" "${YES}" install ohpc-filesystem lmod-ohpc hostname
}

if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		dnf_openeuler
	else
		dnf_rhel
	fi
	adduser ohpc
else
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install ${COMMON_PKGS} awk rpmbuild "${OHPC_RELEASE}"
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install lmod-ohpc
	useradd -m ohpc
fi
