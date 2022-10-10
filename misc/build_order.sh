#!/bin/bash

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
	NAMES=`rpmspec -q ${i} "${FLAGS[@]}" --queryformat '%{name}:' 2> /dev/null`
	# Let's hope the first name is the right one
	NAME=`echo ${NAMES} | cut -d: -f1`
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
python3 misc/build_order.py ${DEPLIST}
