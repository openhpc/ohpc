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

if [ "${PKG_MANAGER}" = "dnf" ]; then
	OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/CentOS_8/x86_64/ohpc-release-2-1.el8.x86_64.rpm"
else
	OHPC_RELEASE="http://repos.openhpc.community/OpenHPC/2/Leap_15/x86_64/ohpc-release-2-1.leap15.x86_64.rpm"
fi

if [ "${FACTORY_VERSION}" != "" ]; then
	FACTORY_REPOSITORY=http://obs.openhpc.community:82/OpenHPC:/2.7:/Factory/
	if [ "${PKG_MANAGER}" = "dnf" ]; then
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}EL_8"
		FACTORY_REPOSITORY_DESTINATION="/etc/yum.repos.d/obs.repo"
	else
		FACTORY_REPOSITORY="${FACTORY_REPOSITORY}Leap_15"
		FACTORY_REPOSITORY_DESTINATION="/etc/zypp/repos.d/obs.repo"
	fi
	FACTORY_REPOSITORY="${FACTORY_REPOSITORY}/OpenHPC:2.7:Factory.repo"
fi

COMMON_PKGS="wget python3"

if [ "${PKG_MANAGER}" = "dnf" ]; then
	loop_command "${PKG_MANAGER}" -y install ${COMMON_PKGS} epel-release dnf-plugins-core git rpm-build gawk "${OHPC_RELEASE}"
	loop_command "${PKG_MANAGER}" config-manager --set-enabled powertools
	loop_command "${PKG_MANAGER}" config-manager --set-enabled devel
	if [ "${FACTORY_VERSION}" != "" ]; then
		loop_command wget "${FACTORY_REPOSITORY}" -O "${FACTORY_REPOSITORY_DESTINATION}"
	fi
	loop_command "${PKG_MANAGER}" -y install lmod-ohpc
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
