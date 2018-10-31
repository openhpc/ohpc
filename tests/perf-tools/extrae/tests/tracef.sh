#!/bin/bash

source ${EXTRAE_DIR}/etc/extrae.sh

export EXTRAE_CONFIG_FILE=./extrae.xml

if [ "$LMOD_FAMILY_MPI" == "mvapich2" ];then
    export LD_PRELOAD=${MPI_DIR}/lib/libmpi.so:${EXTRAE_HOME}/lib/libmpitracef.so # For Fortran apps
fi

## Run the desired program
$*

