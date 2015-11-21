TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

CC  = mpicc
CXX = mpicxx
FC  = mpif90

check_PROGRAMS       = C_parallel
C_parallel_SOURCES   = err_macros.h  nc_tests.h  tst_parallel.c
C_parallel_LDFLAGS   = $(C_LDFLAGS)

check_PROGRAMS      += F90_parallel
F90_parallel_SOURCES = f90tst_parallel.f90
F90_parallel_LDFLAGS = $(FC_LDFLAGS)

TESTS                = test_pnetcdf

