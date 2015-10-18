#!/bin/bash
#
# Test if adios can write and read global arrays over time correctly
# Uses codes from examples/C/global-array and examples/Fortran/global-array
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=7
READPROCS=3

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to . 
cp $TRUNKDIR/examples/C/global-array/no_xml_write_byid .
cp $TRUNKDIR/examples/C/global-array/read_no_xml_write_byid .

echo "Run C no_xml_write_byid"
ls -l ./no_xml_write_byid
echo $MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./no_xml_write_byid
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./no_xml_write_byid
EX=$?
if [ ! -f no_xml_write_byid.bp ]; then
    echo "ERROR: C version of no_xml_write_byid failed. No BP file is created. Exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls -lav no_xml_write_byid.bp -d -n 10 | grep -v -e endianness -e 'file size' > c_bpls.txt
diff -q c_bpls.txt $SRCDIR/reference/no_xml_write_byid_bpls.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of no_xml_write_byid produced a file different from the reference."
    echo "Compare \"bpls -lav $PWD/no_xml_write_byid.bp -d -n 10 | grep -v -e endianness -e 'file size'\" to reference $SRCDIR/reference/no_xml_write_byid_bpls.txt"
    exit 1
fi

echo "Run C read_no_xml_write_byid"
$MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./read_no_xml_write_byid > c_read.txt
EX=$?
if [ $? != 0 ]; then
    echo "ERROR: C version of read_no_xml_write_byid exited with $EX"
    echo "Check $PWD/c_read.txt"
    exit 1
fi

echo "Check output"
diff -q c_read.txt $SRCDIR/reference/no_xml_write_byid_read.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of read_no_xml_write_byid produced a file different from the reference."
    echo "$PWD/c_read.txt to reference $SRCDIR/reference/no_xml_write_byid_read.txt"
    exit 1
fi


if [ $HAVE_FORTRAN != yes ]; then
    exit 0
fi
# run the Fortran tests too if available

mv no_xml_write_byid.bp no_xml_write_byid_c.bp
cp $TRUNKDIR/examples/Fortran/global-array/no_xml_write_byid no_xml_write_byid_f

echo "Run Fortran no_xml_write_byid"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./no_xml_write_byid_f
EX=$?
if [ ! -f no_xml_write_byid.bp ]; then
    echo "ERROR: Fortran version of no_xml_write_byid failed. No BP file is created. Exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls -lav no_xml_write_byid.bp -d -n 10 | grep -v -e endianness -e 'file size' > f_bpls.txt
diff -q f_bpls.txt $SRCDIR/reference/no_xml_write_byid_f_bpls.txt
if [ $? != 0 ]; then
    echo "ERROR: Fortran version of no_xml_write_byid produced a file different from the reference."
    echo "Compare \"bpls -lav $PWD/no_xml_write_byid.bp -d -n 10 | grep -v -e endianness -e 'file size'\" to reference $SRCDIR/reference/no_xml_write_byid_f_bpls.txt"
    exit 1
fi


