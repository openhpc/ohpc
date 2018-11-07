#!/bin/bash

source ${EXTRAE_DIR}/etc/extrae.sh

arch=`uname -m`

if [ "$arch" == "aarch64" ];then
    export EXTRAE_CONFIG_FILE=./extrae-nopapi.xml
else
    export EXTRAE_CONFIG_FILE=./extrae.xml
fi

if [ "$LMOD_FAMILY_MPI" == "mvapich2" ];then
    export LD_PRELOAD=${MPI_DIR}/lib/libmpi.so:${EXTRAE_HOME}/lib/libmpitracef.so
else
    export LD_PRELOAD=${EXTRAE_HOME}/lib/libmpitracef.so
fi

# Run the desired program
$*

