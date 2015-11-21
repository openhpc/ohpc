#!/bin/bash
#
# Test if adios can write variables alternating in multiple steps
# and then it can read them correctly
# Uses ../programs/write_alternate
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
cp $SRCDIR/programs/write_alternate .
cp $SRCDIR/programs/write_alternate.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run write_alternate"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./write_alternate
EX=$?
if [ ! -f write_alternate.bp ]; then
    echo "ERROR: write_alternate failed at creating the BP file, write_alternate.bp. Exit code=$EX"
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: write_alternate failed with exit code=$EX"
    exit 1
fi

