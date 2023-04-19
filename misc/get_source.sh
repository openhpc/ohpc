#!/bin/bash

if [ $# -ne 1 ]; then
	echo "${0} requires the name of the spec file as parameter."
	exit 1
fi

# If running on Fedora special defines are needed
DISTRO=$(rpm --eval '0%{?fedora}')

if [ "${DISTRO}" != "0" ]; then
	FLAGS=(--undefine fedora --define "rhel 8")
fi

PATTERN=${1}

IFS=$'\n'

find . -name "${PATTERN}" -print0 | while IFS= read -r -d '' file
do
	if [ ! -f "${file}" ]; then
		echo "${file} is not a file. Skipping."
		continue
	fi

	echo "${file}"

	DIR=$(dirname "${file}")
	pushd "${DIR}" > /dev/null || exit 1
	BASE=$(basename "${file}")

	SOURCES=$(rpmspec --parse --define '_sourcedir ../../..' "${FLAGS[@]}" "${BASE}" | grep Source)
	for s in ${SOURCES}; do
		echo "${s}"
		# Remove the Source header
		u=$(awk '{ print $2 }' <<< "${s}")
		# Special handler for tests and docs
		if [[ "${u}" == "docs-ohpc.tar" || "${u}" == "tests-ohpc.tar" ]]; then
            if [ ! -f ../SOURCES/"${u}" ]; then
                echo Creating "${u}"
				SOURCEDIR="${PWD%/*}"/SOURCES
				mkdir -p "${SOURCEDIR}"
                tar -C ../../../.. -cf "${SOURCEDIR}/${u}" --xform="s#^#${u%.tar}/#" "${u%-ohpc.tar}"
                continue
            fi
        fi
		# Ignore everything else that's not an http/https download
		if [[ "${u}" != "http"* ]]; then
			continue
		fi
		echo "Trying to wget ${u}"
		# Try to download only if newer
		WGET=$(wget -N -nv -P "../SOURCES" "${u}" 2>&1)
		# Handling for github URLs with #/ or #$/
		if grep -E "#[$]?/" <<< "${u}"; then
			MV_SOURCE=$(echo "${WGET}" | tail -1 | cut -d\  -f6 | sed -e 's/^"//' -e 's/"$//')
			MV_DEST=../SOURCES/$(basename "${u}")
			mv "${MV_SOURCE}" "${MV_DEST}"
		fi
	done

	popd > /dev/null || exit 1
done
