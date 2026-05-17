
CC  = mpicc
CXX = mpicxx
FC  = mpif90

check_PROGRAMS       = C_parallel
C_parallel_SOURCES   = err_macros.h  nc_tests.h  tst_parallel.c
C_parallel_LDADD     = $(C_LDFLAGS)


if ALL_ENABLED
check_PROGRAMS      += F90_parallel
F90_parallel_SOURCES = f90tst_parallel.f90
F90_parallel_LDADD   = $(FC_LDFLAGS)
endif

TESTS                = test_pnetcdf
