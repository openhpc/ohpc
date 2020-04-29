TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS          = CXX_mpi_test
CXX_mpi_test_LDFLAGS    = $(LDFLAGS)
CXX_mpi_test_SOURCES    = CXX_mpi_test.cpp

check_PROGRAMS         += F_test
F_test_LDFLAGS          = $(LDFLAGS)
F_test_SOURCES          = F_test.f

check_PROGRAMS         += C_mpi_test
C_mpi_test_LDFLAGS      = $(LDFLAGS)
C_mpi_test_SOURCES      = C_mpi_test.c

check_PROGRAMS         += C_hybrid_test
C_hybrid_test_LDFLAGS   = $(LDFLAGS)
C_hybrid_test_SOURCES   = C_hybrid.c

TESTS                   = test_module
TESTS                  += rm_execution
