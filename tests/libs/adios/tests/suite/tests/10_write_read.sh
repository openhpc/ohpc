#!/bin/bash
#
# Test if adios can write-read-write-read correctly
# Uses ../programs/write_read
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=3

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $SRCDIR/programs/write_read .
cp $SRCDIR/programs/write_read.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run write_read"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./write_read
EX=$?
if [ ! -f write_read_1.bp ]; then
    echo "ERROR: write_read failed at creating the 1st BP file, write_read_1.bp. Exit code=$EX"
    exit 1
fi

if [ ! -f write_read_2.bp ]; then
    echo "ERROR: write_read failed at creating the 2nd BP file, write_read_2.bp. Exit code=$EX"
    exit 1
fi

if [ $EX != 0 ]; then
    echo "ERROR: write_read failed with exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls write_read_1.bp > c_bpls1.txt
diff -q c_bpls1.txt $SRCDIR/reference/write_read_bpls.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of write_read produced a file different from the reference."
    echo "Compare \"bpls $PWD/write_read_1.bp\" to reference $SRCDIR/reference/write_read_bpls.txt"
    exit 1
fi

$TRUNKDIR/utils/bpls/bpls write_read_2.bp > c_bpls2.txt
diff -q c_bpls2.txt c_bpls1.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of write_read produced two different files in two steps. They should have the same list of variables"
    echo "Compare \"bpls $PWD/write_read_1.bp\" to \"bpls $PWD/write_read_2.bp\""
    exit 1
fi
