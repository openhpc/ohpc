#!/bin/bash
#
# Run this script in an interactive-job environment, with 16 cores at least.
# Define MPIRUN and NP_MPIRUN environment variables for running parallel programs
#   like mpirun and -np  or  aprun and -n
#
# Run the script in the job directory with full path.
#
# Log output of each test can be found in ./log/
#

# default values
XXX=`which aprun &>/dev/null`
if [ $? == 0 ]; then
    MPIRUN="aprun -q"
    NP_MPIRUN=-n
else
    MPIRUN=mpirun
    NP_MPIRUN=-np
fi
KEEPOUTPUT=no
MAXPROCS=128

function add_transform_to_xmls() {
  if [[ $TRANSFORM ]]; then
    sed -i -e '/transform=/!s|\(<var .*dimensions=.*\)/>|\1 transform="'$TRANSFORM'" />|' ./*.xml
  fi
}
# Make this function accessible to child processes (i.e., the actual tests)
export -f add_transform_to_xmls    

EXEOPT=""

function Usage() {
    echo "
Usage:  <path>/`basename $0` [-m runcmd] [-n "-np"] [-p procs] [-h] [-k]
                             [pattern [pattern2 pattern3 ...]]
  <path>    is used to find all the test codes, inputs and reference outputs. 
            codes and inputs will be copied to the job directory
  pattern   if given, only those tests are executed, that match 
            'tests/*<pattern>*.sh';  otherwise all tests are executed.
            e.g. arguments   1[0-3] attr global   will execute all tests that
            have any of the strings 10,11,12,13,attr or global in their name
            (and end with .sh)
  OPTIONS
     -m runcmd  command to start an MPI program. Default: $MPIRUN
     -n runopt  option to runcmd to specify number of cores. Default: $NP_MPIRUN
     -p procs   Run only those tests that use less up to 'procs' processes. 
                Default: $MAXPROCS
     -k         Do not remove logs and work dir of successful tests.
     -t xform   Run tests with transform 'xform' applied to all non-scalar variables
     -e exeopt  option to runcmd to specify executable. Default: empty. 
                BG/Q interactive jobs requires '--exe' before an executable.
     -h         Print this help.
"
}

#####################
# Process arguments #
#####################
    
# process option arguments
while getopts ":m:n:p:t:e:kh" Option
do  
  case $Option in
        m) MPIRUN=$OPTARG;;
        n) NP_MPIRUN=$OPTARG;;
        p) MAXPROCS=$OPTARG;;
        k) KEEPOUTPUT=yes;;
        t) TRANSFORM=$OPTARG;;
        e) EXEOPT=$OPTARG;;
        h) Usage; exit 0;;
        *) echo "Invalid option $Option"; Usage; exit 255;;   # DEFAULT
  esac
done
shift $(($OPTIND - 1))

# get source path
SRCDIR=`dirname $0`
TRUNKDISTANCE=../..

if [ "${SRCDIR:0:1}" != "/" ]; then
    echo "WARNING: Jobs on some systems do not have access to any directory but to "
    echo "  parallel file systems. If the tests fail because of this, run this script"
    echo "  from a directory on the parallel file system and the script will copy the"
    echo "  data automatically."
    echo " "
fi

# check if Fortran codes were built
S=`grep BUILD_FORTRAN_TRUE $SRCDIR/$TRUNKDISTANCE/Makefile`
if [ -z ${S#BUILD_FORTRAN_TRUE = } ]; then
    HAVE_FORTRAN=yes
else
    echo "WARNING: Fortran binaries are not built, so test will not use them"
    HAVE_FORTRAN=no
fi

# Print info before running tests
echo "Settings:"
echo "  Test source directory:  $SRCDIR"
echo "  Run command:            $MPIRUN"
echo "  Run command np option:  $NP_MPIRUN"
echo "  Max. processes to use:  $MAXPROCS"
echo "  Keep test output:       $KEEPOUTPUT"
echo "  Execute option:         $EXEOPT"

# find and list tests to be executed
TESTS=
if [ ! -z "$1" ]; then
    while [ ! -z "$1" ]; do
        TESTS1=`ls $SRCDIR/tests/*$1*.sh`
        if [ -z "$TESTS1" ]; then
            echo "ERROR: Did not find any test with name \"$1\" in $SRCDIR/tests."
        else
            TESTS="$TESTS $TESTS1"
        fi
        shift 1
    done
    if [ -z "$TESTS" ]; then
        exit 1
    fi
    echo "  Tests found: $TESTS"
else
    TESTS=`ls $SRCDIR/tests/*.sh`
    if [ -z "$TESTS" ]; then
        echo "ERROR: Did not find any test in $SRCDIR/tests."
        exit 1
    fi
fi

echo ""
echo "Run tests"

# run tests one by one
for TESTSCRIPT in $TESTS; do
    TEST=`basename ${TESTSCRIPT%%.sh}`
    echo -n "  Test $TEST ... "
    rm -rf log.$TEST work.$TEST
    mkdir -p work.$TEST
    pushd work.$TEST >/dev/null
    if [ "${SRCDIR:0:1}" == "/" ]; then
        TESTSRCDIR=$SRCDIR       
        TRUNKDIR=$SRCDIR/$TRUNKDISTANCE
    else
        # we are one level deeper now
        TESTSRCDIR=../$SRCDIR
        TRUNKDIR=../$SRCDIR/$TRUNKDISTANCE
        TESTSCRIPT=../$TESTSCRIPT
    fi
    # Run the test script with setting the environment for it
    MPIRUN="$MPIRUN" NP_MPIRUN="$NP_MPIRUN" HAVE_FORTRAN="$HAVE_FORTRAN" SRCDIR="$TESTSRCDIR" \
        TRUNKDIR="$TRUNKDIR" MAXPROCS="$MAXPROCS" \
        TRANSFORM="$TRANSFORM" \
        EXEOPT="$EXEOPT" \
        $TESTSCRIPT &> ../log.$TEST
    EX=$?
    popd >/dev/null
    if [ $EX == 0 ]; then
        echo "OK"
        [ "$KEEPOUTPUT" == "no" ] && rm -rf work.$TEST log.$TEST
    elif [ $EX == 77 ];then
        echo "SKIP. Check log.$TEST for the reason"
        [ "$KEEPOUTPUT" == "no" ] && rm -rf work.$TEST 
    else
        echo "FAILED with exit code $EX. Check log.$TEST for details and work.$TEST/ for outputs."
    fi
done




