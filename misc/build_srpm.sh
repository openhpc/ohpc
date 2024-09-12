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

SPEC=$1

if [ $# -ge 2 ]; then
	COMPILER_FAMILY=$2
else
	COMPILER_FAMILY=gnu14
fi

if [ $# -ge 3 ]; then
	MPI_FAMILY=$3
else
	MPI_FAMILY=openmpi5
fi

if [ $# -eq 4 ]; then
	MPI_DEPENDENT=$4
else
	MPI_DEPENDENT=1
fi

if [ ! -e "${SPEC}" ]; then
	echo "Spec file ${SPEC} does not exist. Exiting."
	exit 1
fi

MACROS=components/OHPC_macros

if [ ! -e "${MACROS}" ]; then
	echo -n "This script expects to be started in the top-level OpenHPC git"
	echo " checkout directory."
	echo -n "Checking for '${MACROS}' failed and"
	echo " therefore exiting."
	exit 1
fi

# shellcheck disable=SC1091
. misc/shell-functions

ROOT=$(pwd)

BASE=$(basename "${SPEC}")
DIR=$(dirname "${SPEC}")
echo "Building SRPM for ${SPEC}"

prepare_git_tree "${DIR}"

# Try to build the SRPM
SRPM=$(build_srpm "${SPEC}" "${COMPILER_FAMILY}" "${MPI_FAMILY}" "${MPI_DEPENDENT}")
RESULT=$?
if [ "${RESULT}" == "0" ]; then
	echo "${SRPM}"
	exit 0
fi

echo "Building the SRPM for ${BASE} failed."
echo "Trying to fetch Source0"
"${ROOT}"/misc/get_source.sh "${BASE}"

# Let's hope fetching the sources worked and retry building the SRPM
SRPM=$(build_srpm "${SPEC}" "${COMPILER_FAMILY}" "${MPI_FAMILY}" "${MPI_DEPENDENT}")
RESULT=$?

if [ "${RESULT}" == "1" ]; then
	echo "Still got an error building SRPM for ${BASE}"
	echo "Giving up, this needs to be fixed manually"
	exit 1
fi

echo "${SRPM}"
