#!/bin/bash
#
# Test using one dimension scalar twice with different values for two arrays
# Uses ../programs/reuse_dim
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=1

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $SRCDIR/programs/reuse_dim .

echo "Run reuse_dim"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./reuse_dim
EX=$?
if [ ! -f reuse_dim.bp ]; then
    echo "ERROR: reuse_dim failed at creating the BP file, reuse_dim.bp. Exit code=$EX"
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: reuse_dim failed with exit code=$EX"
    exit 1
fi

