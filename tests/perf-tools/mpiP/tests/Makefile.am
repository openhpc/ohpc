TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1

check_PROGRAMS    = C_test
C_test_SOURCES    = C_test.c

check_PROGRAMS   += CXX_test
CXX_test_SOURCES  = CXX_test.cpp

check_PROGRAMS   += F90_test
F90_test_SOURCES  = F90_test.f90

LIBS=-L${MPIP_LIB} -lmpiP -lbfd -liberty -ldl -lm -lz

TESTS             = test_mpiP

CLEANFILES        = *.mpiP job.*.out
