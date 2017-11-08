#!/bin/bash

PROJECT="ohpc"

if [ ! -e components/OHPC_macros ]; then
	echo -n "This script expects to be started in the top-level OpenHPC git"
	echo " checkout directory."
	echo -n "Checking for 'components/OHPC_macros' failed and"
	echo " therefore exiting."
	exit 1
fi

SOURCEDIR=`rpm --eval '%{_sourcedir}'`

if [ ! -e ${SOURCEDIR}/OHPC_macros ]; then
	echo "${SOURCEDIR}/OHPC_macros not found."
	echo "Please 'cp components/OHPC_macros ${SOURCEDIR}/OHPC_macros'"
	exit 1
fi

cmp -s ${SOURCEDIR}/OHPC_macros components/OHPC_macros

if [ "$?" == "1" ]; then
	echo "WARNING: ${SOURCEDIR}/OHPC_macros and components/OHPC_macros differs."
	echo "This might lead to unexpected/wrong results."
	echo "Continuing anyway."
fi

DEPLIST=`mktemp`

trap "rm -f ${DEPLIST}" EXIT QUIT HUP KILL TERM

# If running on Fedora special defines are needed
DISTRO=`rpm --eval '0%{?fedora}'`

if [ "${DISTRO}" != "0" ]; then
	FLAGS=(--undefine fedora --define "rhel 7")
fi

for i in `find . -name *.spec`; do
	SPEC=`basename ${i}`
	SOURCES="`dirname ${i}`/../SOURCES"
	NAMES=`rpmspec -q ${i} --queryformat '%{name}:' 2> /dev/null`
	# Let's hope the first name is the right one
	NAME=`echo ${NAMES} | cut -d: -f1`

	OIFS=${IFS}
	IFS=$'\n'
	PROVIDES=""
	# Try to find the Provides: from the included modulefiles
	PARSED=`rpmspec -q ${i} "${FLAGS[@]}" --parse 2> /dev/null`
	# Disable glob
	set -f
	for line in ${PARSED}; do
		# Only look at files containing 'ohpc'
		if [[ "${line}" != *"${PROJECT}"* ]]; then
			continue
		fi
		# If the line contains a mkdir, also skip it
		if [[ "${line}" == *"mkdir"* ]]; then
			continue
		fi
		# Search for modules under modulefiles and moduledeps
		# But ignore .version files
		if [[ "${line}" == *"modulefiles"*".version"* || "${line}" == *"moduledeps"*".version"* ]]; then
			continue
		fi
		# Check if providing something by prepend-path
		dep=`grep -E "prepend-path(\s)*MODULEPATH(.)*moduledeps" <<< ${line}`
		if [[ $? -eq 0 ]]; then
			dep=`awk -Fmoduledeps/ '{ print $2 }' <<< ${dep}`
			PROVIDES="${PROVIDES}:${PROJECT}-module(${dep})"
			continue
		fi
		# If it ends in a slash it is not something we are looking for
		if [[ "${line}" == *"/" ]]; then
			continue
		fi
		dep=`grep -E "prepend-path(\s)*MODULEPATH(.)*modulefiles" <<< ${line}`
		if [[ $? -eq 0 ]]; then
			dep=`awk -Fmodulefiles/ '{ print $2 }' <<< ${dep}`
			PROVIDES="${PROVIDES}:${PROJECT}-module(${dep})"
			continue
		fi
		if [[ "${line}" == *"modulefiles"* || "${line}" == *"moduledeps"* ]]; then
			dep=`awk -F'module(files|deps)/' '{ print $2 }' <<< ${line}`

			# As we can test for directory or file in this script
			# and as autotools does not follow the OpenHPC convention
			# of name/version it needs a special case :-(
			if [[ ${dep} == "autotools" ]]; then
				PROVIDES="${PROVIDES}:${PROJECT}-module(${dep})"
				continue
			fi

			# the result needs to have a slash in it to match
			# name/version
			if [[ ${dep} != *"/"* ]]; then
				continue
			fi
			# the result of this should be at least 3 characters long
			deplen=${#dep}
			if [[ ${deplen} -lt 3 ]]; then
				continue
			fi
			PROVIDES="${PROVIDES}:${PROJECT}-module(${dep})"
			PROVIDES="${PROVIDES}:${PROJECT}-module(`dirname ${dep}`)"
		fi
	done
	set +f
	IFS=${OIFS}
	NAMES=`echo "${NAMES}:${PROVIDES}" | sed -e "s,::*,:,"`

	REQ=`rpmspec -q ${i} "${FLAGS[@]}" --requires 2> /dev/null`
	BR=`rpmspec -q ${i} "${FLAGS[@]}" --buildrequires 2> /dev/null`
	for j in ${REQ} ${BR}; do
		if [[ ${j} != *"ohpc"* ]] || [[ ${j} == *"buildroot"* ]]; then
			OIFS=${IFS}
			IFS=':'
			for x in ${NAMES}; do
				# Does not have dependency on an OpenHPC package.
				# Still necessary to track as it could be a
				# dependency itself.
				echo "${SPEC}:${x}:NA" >> ${DEPLIST}
			done
			IFS=${OIFS}
			continue
		fi
		# This is a real dependency on an OpenHPC package.
		echo "${SPEC}:${NAME}:${j}" >> ${DEPLIST}
	done
done

# Dependencies are resolved in python
python misc/build_order.py ${DEPLIST}
