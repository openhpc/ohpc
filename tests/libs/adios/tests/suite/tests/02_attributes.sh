#!/bin/bash
#
# Test if adios can write and read attributes correctly
# Uses codes from examples/C/attributes 
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=5
READPROCS=2

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $TRUNKDIR/examples/C/attributes/attributes_read .
cp $TRUNKDIR/examples/C/attributes/attributes_write .
cp $TRUNKDIR/examples/C/attributes/attributes.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run C attributes_write"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./attributes_write
EX=$?
if [ ! -f attributes.bp ]; then
    echo "ERROR: C version of attributes_write failed. No BP file is created. Exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls -lav attributes.bp | grep -v -e endianness -e 'file size' > c_bpls.txt
diff -q c_bpls.txt $SRCDIR/reference/attributes_bpls.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of attributes_write produced a file different from the reference."
    echo "Compare \"bpls -lav $PWD/attributes.bp | grep -v -e endianness -e 'file size'\" to reference $SRCDIR/reference/attributes_bpls.txt"
    exit 1
fi

echo "Run C attributes_read"
$MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./attributes_read | sort > c_read.txt
EX=$?
if [ $? != 0 ]; then
    echo "ERROR: C version of attributes_read failed with exit code $EX"
    exit 1
fi
echo "Check output"
diff -q c_read.txt $SRCDIR/reference/attributes_read.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of attributes_read produced an output different from the reference."
    echo "Compare $PWD/c_read.txt reference $SRCDIR/reference/attributes_read.txt"
    exit 1
fi


