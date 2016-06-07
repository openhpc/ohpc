#!/bin/bash
#
# Test if adios POSIX method 
# Uses codes from SRCDIR/programs/posix_method
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

if [ "$HAVE_FORTRAN" != "yes" ]; then
    echo "Fortran is not available for this ADIOS build. This tests wants to use a Fortran90 code." 
    exit 77 
fi

# copy codes and inputs to . 
cp $SRCDIR/programs/posix_method .
cp $SRCDIR/programs/posix_method.xml .

# Insert transform=X if requested by user
add_transform_to_xmls

echo "Run posix_method"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./posix_method
EX=$?
if [ $EX != 0 ]; then
    echo "ERROR: posix method failed with exit code=$EX"
    exit $EX
fi

if [ ! -f posix_method.bp ]; then
    echo "ERROR: posix_method failed. No BP file posix_method.bp was created."
    exit 1
fi

if [ ! -d posix_method.bp.dir ]; then
    echo "ERROR: posix_method failed. No BP subfile directory posix_method.bp.dir was created."
    exit 1
fi

for ((i=0; i<$PROCS; i++)); do
    if [ ! -f posix_method.bp.dir/posix_method.bp.$i ]; then
        echo "ERROR: posix_method failed. No BP subfile posix_method.bp.dir/posix_method.bp.$i was created."
        exit 1
    fi
done



