#!/bin/bash

# shellcheck disable=SC2086

set -x
set -e

FACTORY_VERSION=3.1

if [ ! -e /etc/os-release ]; then
	echo "Cannot detect OS without /etc/os-release"
	exit 1
fi

# shellcheck disable=SC1091
. /etc/os-release

PKG_MANAGER=zypper
COMMON_PKGS="wget python3 jq"
UNAME_M=$(uname -m)
YES="-n"

OBS_SERVER="http://obs.openhpc.community:82/"
PROJECT="OpenHPC3:"
REPOSITORY_URL="http://repos.openhpc.community/OpenHPC/3/"

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
		OHPC_RELEASE="${REPOSITORY_URL}openEuler_22.03/${UNAME_M}/ohpc-release-3-1.oe2203.${UNAME_M}.rpm"
	else
		OHPC_RELEASE="${REPOSITORY_URL}EL_9/${UNAME_M}/ohpc-release-3-1.el9.${UNAME_M}.rpm"
	fi
else
	OHPC_RELEASE="${REPOSITORY_URL}Leap_15/${UNAME_M}/ohpc-release-3-1.leap15.${UNAME_M}.rpm"
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
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/${PROJECT}${FACTORY_VERSION}:Factory.repo"
fi

dnf_rhel() {
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	loop_command "${PKG_MANAGER}" config-manager --set-enabled crb
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" install lmod-ohpc bats
}

dnf_openeuler() {
	loop_command "${PKG_MANAGER}" "${YES}" install ${COMMON_PKGS} git dnf-plugins-core rpm-build gawk "${OHPC_RELEASE}"
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command wget -P /etc/yum.repos.d/ https://eur.openeuler.openatom.cn/coprs/mgrigorov/OpenHPC/repo/openeuler-22.03_LTS_SP3/mgrigorov-OpenHPC-openeuler-22.03_LTS_SP3.repo
	loop_command "${PKG_MANAGER}" "${YES}" install ohpc-filesystem lmod-ohpc hostname bats
}

if [ "${PKG_MANAGER}" = "dnf" ]; then
	if [ "${ID}" = "openEuler" ]; then
		dnf_openeuler
	else
		dnf_rhel
	fi
	adduser ohpc
else
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install ${COMMON_PKGS} awk rpmbuild bats "${OHPC_RELEASE}"
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" "${YES}" --no-gpg-checks install lmod-ohpc
	useradd -m ohpc
fi
