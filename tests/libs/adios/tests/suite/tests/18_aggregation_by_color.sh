#!/bin/bash
#
# Test if adios aggregates data by color properly.
# Uses the example code from examples/C/global-array/adios_global_aggregate_by_color
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
cp $TRUNKDIR/examples/C/global-array/adios_global_aggregate_by_color .

echo "Run C adios_global_aggregate_by_color"
$MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./adios_global_aggregate_by_color
EX=$?
if [ ! -f adios_global_aggregate_by_color.bp ] \
|| [ ! -f adios_global_aggregate_by_color.bp.dir/adios_global_aggregate_by_color.bp.0 ] \
|| [ ! -f adios_global_aggregate_by_color.bp.dir/adios_global_aggregate_by_color.bp.1 ]; then
    echo "ERROR: adios_global_aggregate_by_color failed. No BP file is created. Exit code=$EX"
    exit 1
fi
