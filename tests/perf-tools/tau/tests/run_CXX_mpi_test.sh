#!/bin/bash

ARGS=$1
NODES=${2:-2}
TASKS=${3:-4}

dimX=500
dimY=500
numGridsX=$NODES
numGridsY=$TASKS
numInitPerturbations=4

tau_exec ./CXX_mpi_test ${dimX} ${dimY} ${numGridsX} ${numGridsY} ${numInitPerturbations}
