#!/bin/bash
#
# Test if staged read method functions correctly
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

PROCS=32
READPROCS=4

if [ $MAXPROCS -lt $PROCS ]; then
    echo "WARNING: Needs $PROCS processes at least"
    exit 77  # not failure, just skip
fi

# copy codes and inputs to .
cp $SRCDIR/programs/adios_amr_write .
cp $SRCDIR/programs/adios_amr_write.xml .
cp $SRCDIR/programs/adios_staged_read .
cp $SRCDIR/programs/adios_staged_read_v2 .

# Insert transform=X if requested by user
add_transform_to_xmls

for ((m=1; m <= 2 ; m++))
do
    echo "Run C adios_amr_write"
    ls -l ./adios_amr_write
    echo $MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./adios_amr_write $m
    rm -f *.bp
    $MPIRUN $NP_MPIRUN $PROCS $EXEOPT ./adios_amr_write $m
    EX=$?
    ls -l ./adios_amr_write.bp
    if [ ! -f adios_amr_write.bp ]; then
        echo "ERROR: C version of adios_amr_write failed. No BP file is created. Exit code=$EX"
        exit 1
    fi

    echo "Run C adios_staged_read"
    ls -l ./adios_staged_read
    export num_aggregators=2
    export chunk_size=64
    for ((n=1; n <= 5 ; n++))
    do
        echo $MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./adios_staged_read $n
        $MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./adios_staged_read $n| grep -v aggregator | grep [0-9] > 08_amr_write_read_$n.txt
        EX=$?
       echo "Check output with reference"
       diff -q 08_amr_write_read_$n.txt $SRCDIR/reference/amr_write_read_$n.txt
       if [ $? != 0 ]; then
           echo "ERROR: C version of adios_staged_read produced data different from the reference."
           exit 1
       fi
    done

    echo "Run C adios_staged_read_v2"
    ls -l ./adios_staged_read_v2
    export num_aggregators=2
    export chunk_size=64
    for ((n=1; n <= 5 ; n++))
    do
        echo $MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./adios_staged_read_v2 $n
        $MPIRUN $NP_MPIRUN $READPROCS $EXEOPT ./adios_staged_read_v2 $n| grep -v aggregator | grep [0-9] > 08_amr_write_read_$n.txt
        EX=$?
       echo "Check output with reference"
       diff -q 08_amr_write_read_$n.txt $SRCDIR/reference/amr_write_read_$n.txt
       if [ $? != 0 ]; then
           echo "ERROR: C version of adios_staged_read_v2 produced data different from the reference."
           exit 1
       fi
    done

done 
