#!/bin/bash
#
# Test if the ADIOS query framework can successfully perform a range of *serial* queries on simple, pre-defined datasets
# Parallel query tests are tested in *TODO*
#
# Uses:
# - tests/C/query/common/build_indexed_dataset (executable, produces the pre-defined datasets)
# - tests/C/query/common/xml-testcases/*/* (custom XML files describing interesting queries over the predefined datasets)
#   - e.g. tests/C/query/common/xml-testcases/DS1/simple-query.xml
# - tests/C/query/common/compute_expected_query_results (executable, runs queries via sequential scan over a BP file)
#
# Environment variables set by caller:
# MPIRUN        Run command
# NP_MPIRUN     Run commands option to set number of processes
# MAXPROCS      Max number of processes allowed
# HAVE_FORTRAN  yes or no
# SRCDIR        Test source dir (.. of this script)
# TRUNKDIR      ADIOS trunk dir

# This function prints a message to stderr, then exits with the
# given error code (defaults to 1). Usage: die <msg> <code>
function die() {
  EC=${2-1}
  echo "$1" >&2
  exit $EC
}

# mpirun for serial command

USEPROCS=$MAXPROCS
if [[ $USEPROCS -gt 4 ]]; then USEPROCS=4; fi 
MPIRUN_CMD="$MPIRUN $NP_MPIRUN $USEPROCS"

# Basic directory structure
TEST_PROGRAMS_DIR="$TRUNKDIR/tests/suite/programs"
UTILS_DIR="$TRUNKDIR/utils"

# Some external tools to use
DATASET_BUILDER_EXE_BASENAME="build_standard_dataset"
DATASET_TESTER_EXE_BASENAME="transforms_writeblock_read"
LIST_METHODS_EXE_BASENAME="list_methods"

DATASET_BUILDER_EXE_PATH="$TEST_PROGRAMS_DIR/$DATASET_BUILDER_EXE_BASENAME"
DATASET_TESTER_EXE_PATH="$TEST_PROGRAMS_DIR/$DATASET_TESTER_EXE_BASENAME"
LIST_METHODS_EXE_PATH="$UTILS_DIR/list_methods/$LIST_METHODS_EXE_BASENAME"

# Check for the executability of all executables that we need
[ -f "$DATASET_BUILDER_EXE_PATH" -a -x "$DATASET_BUILDER_EXE_PATH" ] || die "ERROR: $DATASET_BUILDER_EXE_PATH is not executable"
[ -f "$DATASET_TESTER_EXE_PATH"  -a -x "$DATASET_TESTER_EXE_PATH"  ] || die "ERROR: $DATASET_TESTER_EXE_PATH is not executable"
[ -f "$LIST_METHODS_EXE_PATH"    -a -x "$LIST_METHODS_EXE_PATH"    ] || die "ERROR: $LIST_METHODS_EXE_PATH is not executable"

# Copy the external tools to the working directory for convenience
DATASET_BUILDER_EXE_LOCAL="./$DATASET_BUILDER_EXE_BASENAME"
DATASET_TESTER_EXE_LOCAL="./$DATASET_TESTER_EXE_BASENAME"
cp $DATASET_BUILDER_EXE_PATH $DATASET_BUILDER_EXE_LOCAL
cp $DATASET_TESTER_EXE_PATH $DATASET_TESTER_EXE_LOCAL

# All pre-defined dataset IDs (which can be extracted from build_indexed_dataset)
ALL_DATASET_IDS=" \
  DS-1D \
  DS-2D \
  DS-3D \
  DS-particle \
"

ALL_TRANSFORMS=$( \
  $LIST_METHODS_EXE_PATH |
  awk '
    /^Available/{
      transforms = ($2 == "data")
    }
    {
      if (transforms) {
        if (skippedheader) {
          gsub("\"","",$1)
          print $1
        } else {
          skippedheader = 1
        }
      }
    }
  ' |
  grep -v -eisobar -eszip
)

echo "NOTE: Testing with the following installed data transformations: $ALL_TRANSFORMS" 

function invoke_dataset_builder() {
  local DSID="$1"
  local DSOUTPUT="$2"
  local TRANSFORM="$3"
  [[ $# -eq 3 ]] || die "ERROR: Internal testing error, invalid parameters to invoke_dataset_builder: $*"
  
  # Use Identity because it's built-in, known-good and and should be sufficient
  # to test writeblock reads over transformed data
  
  set -o xtrace
  $DATASET_BUILDER_EXE_LOCAL "$DSID" "$DSOUTPUT" "$TRANSFORM" ||
    die "ERROR: $DATASET_BUILDER_EXE_LOCAL failed with exit code $? (on dataset $DSID, outputting to $DSOUTPUT, using transform argument $TRANSFORM_ARG)"
  set +o xtrace
  
  # Rename the ADIOS XML used for writing, so it's more clear what it is used
  # for (i.e., not a query test specification XML)
  mv "$DSOUTPUT.xml" "$DSOUTPUT.create.xml"
  
  # Ensure the dataset was actually produced
  local TRANSFORMED_DS="$DSOUTPUT.bp"
  [ -f "$TRANSFORMED_DS" ] ||
    die "ERROR: $DATASET_BUILDER_EXE_LOCAL did not produce expected output BP file \"$TRANSFORMED_DS\""
}

function build_datasets() {
  local DSID
  local TRANSFORM
  
  for TRANSFORM in $ALL_TRANSFORMS; do
    local TRANSFORM_DIR="$TRANSFORM"
    mkdir -p $TRANSFORM_DIR
    for DSID in $ALL_DATASET_IDS; do
      invoke_dataset_builder "$DSID" "$TRANSFORM_DIR/$DSID" "$TRANSFORM"
    done
  done
}



function test_datasets() {
  local DSID

  for DSID in $ALL_DATASET_IDS; do
    for TRANSFORM in $ALL_TRANSFORMS; do    
      local DS_FILE="$TRANSFORM/$DSID.bp"

      echo
      echo "=== TESTING WRITEBLOCK READS ON DATASET $DSID TRANSFORMED WITH TRANSFORM $TRANSFORM ==="
      echo
      set -o xtrace
      $MPIRUN_CMD "$DATASET_TESTER_EXE_LOCAL" "$DS_FILE" ||
          die "ERROR: $DATASET_TESTER_EXE_LOCAL failed on dataset $DSID and transform $TRANSFORM with exit code $?"
      set +o xtrace
    done
  done
}



# FINALLY, CALL THE FUNCTIONS IN SEQUENCE
build_datasets
test_datasets
