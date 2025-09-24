
check_PROGRAMS         = C_mpi_test
C_mpi_test_LDADD       = -lhdf5
C_mpi_test_SOURCES     = C_mpi_test.c

check_PROGRAMS        += F_mpi_test
F_mpi_test_LDADD       = -lhdf5_fortran
F_mpi_test_SOURCES     = F_mpi_test.f90

TESTS                  = rm_execution
TESTS                  += test_module
