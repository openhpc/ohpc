#!/bin/bash

source ./common/TEST_ENV
unset OMP_NUM_THREADS
export OMP_NUM_THREADS=10
if [ "x$ARCH" == "xx86_64" ];then
    lib=callpath-param-papi-pdt-openmp-opari-profile-trace
else
    lib=callpath-param-pdt-openmp-opari-profile-trace
fi
tau_exec -XrunTAUsh-$lib ./C_omp_test
