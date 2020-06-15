#!/bin/bash

source ./common/TEST_ENV
unset OMP_NUM_THREADS
export OMP_NUM_THREADS=4
tau_exec  ./C_hybrid_test

