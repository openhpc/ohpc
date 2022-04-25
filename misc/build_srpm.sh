#!/bin/bash
#  Copyright 2017 Adrian Reber
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

PATTERN=$1

if [ $# -eq 2 ]; then
	MPI_FAMILY=$2
else
	MPI_FAMILY=openmpi4
fi

MACROS=components/OHPC_macros

if [ ! -e ${MACROS} ]; then
	echo -n "This script expects to be started in the top-level OpenHPC git"
	echo " checkout directory."
	echo -n "Checking for '${MACROS}' failed and"
	echo " therefore exiting."
	exit 1
fi

. misc/shell-functions

ROOT=`pwd`

for i in `find . -name "*${PATTERN}*spec"`; do
	BASE=`basename $i`
	DIR=`dirname $i`
	echo "Building SRPM for ${i}"

	prepare_git_tree ${DIR}

	# Try to build the SRPM
	SRPM=`build_srpm ${i} ${MPI_FAMILY}`
	RESULT=$?
	if [ "$RESULT" == "1" ]; then
		echo "Building the SRPM for ${BASE} failed."
		echo "Trying to fetch Source0"
		${ROOT}/misc/get_source.sh ${BASE}
	fi

	# Let's hope fetching the sources worked and retry building the SRPM
	SRPM=`build_srpm ${i} ${MPI_FAMILY}`
	RESULT=$?

	if [ "$RESULT" == "1" ]; then
		echo "Still got an error building SRPM for ${BASE}"
		echo "Giving up, this needs to be fixed manually"
		continue
	fi

	echo ${SRPM}
done
