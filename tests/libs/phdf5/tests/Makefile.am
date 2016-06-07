TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
SUBDIRS                 = ohpc_module

TESTS                  = C_mpi_test
check_PROGRAMS         = C_mpi_test
C_mpi_test_LDFLAGS     = -lhdf5
C_mpi_test_SOURCES     = C_mpi_test.c

TESTS                 += F_mpi_test
check_PROGRAMS        += F_mpi_test
F_mpi_test_LDFLAGS     = -lhdf5_fortran
F_mpi_test_SOURCES     = F_mpi_test.f90

TESTS                  += rm_execution


