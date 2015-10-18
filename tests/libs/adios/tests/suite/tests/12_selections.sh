#!/bin/bash
#
# Test if adios can read data with points selection
# Uses ../programs/selections
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
cp $SRCDIR/programs/selections .

echo "Run selections"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./selections
EX=$?
if [ ! -f selections.bp ]; then
    echo "ERROR: selections failed at creating the BP file, selections.bp. Exit code=$EX"
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: selections failed with exit code=$EX"
    exit 1
fi

