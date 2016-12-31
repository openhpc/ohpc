#!/bin/bash

unset OMP_NUM_THREADS
export OMP_NUM_THREADS=`./get_max_threads`
tau_exec ./C_omp_test
