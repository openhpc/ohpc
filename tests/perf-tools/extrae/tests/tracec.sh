#!/bin/bash

source ${EXTRAE_DIR}/etc/extrae.sh

arch=`uname -m`

if [ "$arch" == "aarch64" ] || [ -n "$SIMPLE_CI" ];then
    export EXTRAE_CONFIG_FILE=./extrae-nopapi.xml
else
    export EXTRAE_CONFIG_FILE=./extrae.xml
fi

export LD_PRELOAD=${EXTRAE_HOME}/lib/libmpitrace.so

# Run the desired program
$*

