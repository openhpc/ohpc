#!/bin/bash

unset OMP_NUM_THREADS
export OMP_NUM_THREADS=10
tau_exec ./C_omp_test
