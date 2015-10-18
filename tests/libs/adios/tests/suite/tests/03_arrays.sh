#!/bin/bash
#
# Test if adios can write and read arrays correctly
# Uses codes from examples/C/arrays 
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=5

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $TRUNKDIR/examples/C/arrays/arrays_read .
cp $TRUNKDIR/examples/C/arrays/arrays_write .
cp $TRUNKDIR/examples/C/arrays/arrays.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run C arrays_write"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./arrays_write
EX=$?
if [ ! -f arrays.bp ]; then
    echo "ERROR: C version of arrays_write failed. No BP file is created. Exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls -lav arrays.bp | grep -v -e endianness -e 'file size' > c_bpls.txt
diff -q c_bpls.txt $SRCDIR/reference/arrays_bpls.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of arrays_write produced a file different from the reference."
    echo "Compare \"bpls -lav $PWD/arrays.bp | grep -v -e endianness -e 'file size'\" to reference $SRCDIR/reference/arrays_bpls.txt"
    exit 1
fi

echo "Run C arrays_read"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./arrays_read | sort > c_read.txt
EX=$?
if [ $? != 0 ]; then
    echo "ERROR: C version of arrays_read failed with exit code $EX"
    exit 1
fi
echo "Check output"
diff -q c_read.txt $SRCDIR/reference/arrays_read.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of arrays_read produced an output different from the reference."
    echo "Compare $PWD/c_read.txt reference $SRCDIR/reference/arrays_read.txt"
    exit 1
fi


