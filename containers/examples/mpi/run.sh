#!/bin/bash

module load gnu13 openmpi5 prun
mpicc mpi.c -o mpi
sbatch sbatch.sh
