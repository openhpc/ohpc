#!/bin/bash

if [ $# -ne 1 ]; then
	echo "${0} requires the name of the spec file as parameter."
	exit 1
fi

PATTERN=${1}

IFS=$'\n'

for i in `find . -name "${PATTERN}"`; do

	if [ ! -f ${i} ]; then
		echo "${i} is not a file. Skipping."
		continue
	fi

	echo ${i}

	pushd `dirname ${i}` > /dev/null
	BASE=`basename ${i}`

	SOURCES=`rpmspec --parse --define '_sourcedir ../SOURCES' --define 'rhel 7' ${BASE} | grep Source`
	for u in ${SOURCES}; do
		echo ${u}
		if [[ "${u}" != *"http"* ]]; then
			continue
		fi
		u=`awk '{ print $2 }' <<< ${u}`
		echo "Trying to get ${u}"
		# Try to download only if newer
		WGET=`wget -N -nv -P ../SOURCES ${u} 2>&1`
		# Handling for github URLs with #/ or #$/
		if grep -E "#[$]?/" <<< ${u}; then
			URL=`echo ${u} | cut -d# -f1`
			mv `echo ${WGET} | cut -d\  -f13 | sed -e 's/^"//' -e 's/"$//'` ../SOURCES/`basename ${u}`
		fi
	done

	popd > /dev/null
done
