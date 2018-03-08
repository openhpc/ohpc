#!/bin/bash

unset OMP_NUM_THREADS
export OMP_NUM_THREADS=10
tau_exec -XrunTAUsh-callpath-param-papi-pdt-openmp-opari-profile-trace ./C_omp_test
