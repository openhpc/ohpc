#!/bin/bash

unset OMP_NUM_THREADS
export OMP_NUM_THREADS=10
tau_exec -XrunTAUsh-callpath-param-pdt-openmp-profile-trace ./C_omp_test
