TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
SUBDIRS                 = ohpc_module

TESTS                   = C_test
check_PROGRAMS          = C_test
C_test_LDFLAGS          = -lfftw3
C_test_SOURCES          = C_test.c

TESTS                  += C_mpi_test
check_PROGRAMS         += C_mpi_test
C_mpi_test_LDFLAGS      = -lfftw3 -lfftw3_mpi
C_mpi_test_SOURCES      = C_mpi_test.c

TESTS                  += C_threads_test
check_PROGRAMS         += C_threads_test
C_threads_test_LDFLAGS  = -lfftw3 -lfftw3_threads
C_threads_test_SOURCES  = C_threads_test.c

TESTS                  += F_test
check_PROGRAMS         += F_test
F_test_LDFLAGS          = -lfftw3
F_test_SOURCES          = F_test.f90

TESTS                  += CXX_omp_test
check_PROGRAMS         += CXX_omp_test
CXX_omp_test_LDFLAGS    = -lfftw3 -lfftw3_omp
CXX_omp_test_SOURCES    = CXX_omp_test.cpp

TESTS                  += rm_execution


