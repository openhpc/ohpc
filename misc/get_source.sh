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

	# .../SOURCES/get_source.sh is an optional "plugin" that could build/fetch component's sources on the fly
	if [ -f ../SOURCES/get_source.sh ]; then
		bash ../SOURCES/get_source.sh
	fi

	BASE=$(basename "${file}")

	SOURCES=$(rpmspec --parse --define '_sourcedir ../../..' "${FLAGS[@]}" "${BASE}" | grep Source)
	for u in ${SOURCES}; do
		echo "${u}"
		if [[ "${u}" != *"http"* ]]; then
			continue
		fi
		u=$(awk '{ print $2 }' <<< "${u}")
		echo "Trying to get ${u}"
		# Handling for github-style URLs with #/ or #$/, e.g.
		#   https://github.com/foo/bar/archive/vX.Y.tar.gz#/bar-X.Y.tar.gz
		# rpm downloads the part before '#' and stores it locally
		# under the name that follows '#/'. The fragment is a
		# client-side-only construct that is never sent to the
		# server, so fetch the URL with it stripped and save
		# straight to the target name via -O, instead of trying to
		# rename the file afterwards by parsing wget's log output
		# (whose format is not the same between wget and wget2).
		if [[ "${u}" =~ \#[$]?/ ]]; then
			FETCH_URL=${u%%#*}
			LOCAL_NAME=$(basename "${u}")
			echo "Trying to get ${FETCH_URL} as ${LOCAL_NAME}"
			wget -nv -O "../SOURCES/${LOCAL_NAME}" "${FETCH_URL}"
		else
			# Try to download only if newer
			wget -N -nv -P ../SOURCES "${u}"
		fi
	done

	popd > /dev/null || exit 1
done
