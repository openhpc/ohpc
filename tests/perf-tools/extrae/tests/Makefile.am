TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS          = mpi_ping
mpi_ping_LDFLAGS        = $(LDFLAGS)
mpi_ping_SOURCES        = mpi_ping.f

CLEANFILES              = pingtmp* mpi_ping.prv

TESTS                   = test_module
TESTS                  += rm_execution
