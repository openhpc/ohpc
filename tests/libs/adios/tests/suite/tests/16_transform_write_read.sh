#!/bin/bash
#
# Test if adios can write and read scalars correctly
# Uses codes from examples/C/scalars and examples/Fortran/scalars 
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run command's option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=16
TEST_NAME=transforms_read_write
PROGRAM_NAME=adios_${TEST_NAME}
OUTPUT_FILENAME=${PROGRAM_NAME}.bp

cp ${SRCDIR}/programs/${PROGRAM_NAME} .
cp ${SRCDIR}/programs/adios_transforms.xml .

echo "Run C ${PROGRAM_NAME}"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./${PROGRAM_NAME}
 
EX=$?
if [ ! -f ${OUTPUT_FILENAME} ]; then
    echo "ERROR: C version of ${PROGRAM_NAME} failed. No BP file is created. Exit code=$EX"
    exit 1
fi

echo "Check output with bpls"
$TRUNKDIR/utils/bpls/bpls -la ${OUTPUT_FILENAME} | grep -v -e endianness -e 'file size' > ${TEST_NAME}_bpls.txt

# check scalars first
echo "  check scalars first"
grep -v "t_" ${TEST_NAME}_bpls.txt > c1.txt
grep -v "t_" $SRCDIR/reference/${TEST_NAME}_bpls.txt > c2.txt
diff -q c1.txt c2.txt
if [ $? != 0 ]; then
    echo "ERROR: C version of ${PROGRAM_NAME} produced a file different from the reference."
    echo "Some of the scalars don't match with the reference. "
    echo "Compare \"bpls -la $PWD/${OUTPUT_FILENAME} | grep -v -e "t_" -e endianness -e 'file size'\" to reference $SRCDIR/reference/${TEST_NAME}_bpls.txt"
    exit 1
fi
rm c1.txt c2.txt

for VAR in t_none t_identity t_aplod t_isobar
do
    echo "  $VAR"
    grep "$VAR"  ${TEST_NAME}_bpls.txt > c1.txt
    if [ $? == 0 ]; then
        grep "$VAR"  $SRCDIR/reference/${TEST_NAME}_bpls.txt > c2.txt
        if [ $? != 0 ]; then
            echo "ERROR: C version of ${PROGRAM_NAME} produced a file different from the reference."
            echo "Variable $VAR does not match with the reference. "
            echo "Compare \"bpls -la $PWD/${OUTPUT_FILENAME} | grep -v -e "t_" -e endianness -e 'file size'\" to reference $SRCDIR/reference/${TEST_NAME}_bpls.txt"
            exit 1
        fi
    else
        echo "  $VAR is not in the output. Skip check."
    fi
    rm -f c1.txt c2.txt
done


