#!/bin/bash
set -e

module load gnu15 openmpi5 prun
mpicc mpi.c -o mpi
sbatch sbatch.sh
