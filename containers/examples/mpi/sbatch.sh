#!/bin/bash
#SBATCH -n 8
#SBATCH --tasks-per-node=1

set -e

module load gnu15 openmpi5 prun

prun ./mpi
