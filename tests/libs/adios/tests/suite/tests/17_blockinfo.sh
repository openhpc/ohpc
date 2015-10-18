#!/bin/bash
#
# Test if adios can read correctly the per-block information of 
# a global array that was written with multi-block per process  
# Uses ../programs/blocks.c
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=2

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $SRCDIR/programs/blocks .

echo "Run blocks"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./blocks 
EX=$?
if [ ! -f blocks.bp ]; then
    echo "ERROR: blocks failed at creating the BP file, blocks.bp. Exit code=$EX."
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: blocks failed reading per-block information from blocks.bp with $EX errors."
    exit 1
fi

