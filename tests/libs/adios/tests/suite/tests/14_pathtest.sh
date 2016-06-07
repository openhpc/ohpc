#!/bin/bash
#
# Test different path+name combinations in ADIOS xml
# Uses ../programs/path_test
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
cp $SRCDIR/programs/path_test .
cp $SRCDIR/programs/path_test.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run path_test"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./path_test
EX=$?
if [ ! -f path_test_1.bp ]; then
    echo "ERROR: path_test failed at creating the BP file, path_test_1.bp. Exit code=$EX"
    exit 1
fi

if [ ! -f path_test_2.bp ]; then
    echo "ERROR: path_test failed at creating the BP file, path_test_2.bp. Exit code=$EX"
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: path_test failed with exit code=$EX"
    exit 1
fi

