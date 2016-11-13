TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1


#if HAVE_OHPC_INTEL_COMPILER
#SUBDIRS = tbb
#endif

check_PROGRAMS          = CXX_mpi_test
CXX_mpi_test_LDFLAGS    = $(LDFLAGS)
CXX_mpi_test_SOURCES    = CXX_mpi_test.cpp

check_PROGRAMS         += F_test
F_test_LDFLAGS          = $(LDFLAGS)
F_test_SOURCES          = F_test.f
TESTS                   = F_test

check_PROGRAMS         += C_mpi_test
C_mpi_test_LDFLAGS      = $(LDFLAGS)
C_mpi_test_SOURCES      = C_mpi_test.c
TESTS                  += C_mpi_test

check_PROGRAMS         += C_omp_test
#C_omp_test_CFLAGS       = "-O0 -g "
C_omp_test_CC           = "taucc -fopenmp"
C_omp_test_LDFLAGS      = $(LDFLAGS)
C_omp_test_SOURCES      = C_omp_test.c
TESTS                  += C_omp_test

TESTS                  += test_module
TESTS                  += rm_execution


