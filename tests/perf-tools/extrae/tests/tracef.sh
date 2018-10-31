#!/bin/bash

source ${EXTRAE_DIR}/etc/extrae.sh

export EXTRAE_CONFIG_FILE=./extrae.xml

if [ "$LMOD_FAMILY_MPI" == "mvapich2" ];then
    export LD_PRELOAD=${MPI_DIR}/lib/libmpi.so:${EXTRAE_HOME}/lib/libmpitracef.so
else
    export LD_PRELOAD=${EXTRAE_HOME}/lib/libmpitracef.so
fi

## Run the desired program
$*

