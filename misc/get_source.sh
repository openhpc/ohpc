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

prepare_docs_ohpc_tar() {
	rm -rf docs-ohpc /tmp/ohpc-for-docs
	mkdir -p docs-ohpc
	# Fetch the full Git history so that `git describe` works later
	git clone https://github.com/openhpc/ohpc /tmp/ohpc-for-docs
	cp -r /tmp/ohpc-for-docs/.git docs-ohpc/
	# Copy the local docs/
	cp -r docs docs-ohpc/
	
	tar cf components/admin/docs/SOURCES/docs-ohpc.tar docs-ohpc
	rm -rf docs-ohpc
}

if [[ "${PATTERN}" == "docs.spec" ]]; then
	prepare_docs_ohpc_tar
fi

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
	for u in ${SOURCES}; do
		echo "${u}"
		if [[ "${u}" != *"http"* ]]; then
			continue
		fi
		u=$(awk '{ print $2 }' <<< "${u}")
		echo "Trying to get ${u}"
		# Try to download only if newer
		WGET=$(wget -N -nv -P ../SOURCES "${u}" 2>&1)
		# Handling for github URLs with #/ or #$/
		if grep -E "#[$]?/" <<< "${u}"; then
			MV_SOURCE=$(echo "${WGET}" | tail -1 | cut -d\  -f6 | sed -e 's/^"//' -e 's/"$//')
			MV_DEST=../SOURCES/$(basename "${u}")
			mv "${MV_SOURCE}" "${MV_DEST}"
		fi
	done

	popd > /dev/null || exit 1
done
