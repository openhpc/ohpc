TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS          = heat_mpi
heat_mpi_LDFLAGS        = $(LDFLAGS) -lm
heat_mpi_SOURCES        = heat_mpi.c

CLEANFILES              = pingtmp* heat_mpi.prv

TESTS                   = test_module
TESTS                  += rm_execution
