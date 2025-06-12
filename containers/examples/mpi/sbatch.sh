#!/bin/bash

#SBATCH -n 8
#SBATCH --tasks-per-node=1

module load gnu13 openmpi5 prun

prun ./mpi
