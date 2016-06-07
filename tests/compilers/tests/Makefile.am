TESTS_ENVIRONMENT        = BATS_NO_SUMMARY=1

TESTS                    = C_test
check_PROGRAMS           = C_test
C_test_SOURCES           = C_test.c

TESTS                   += CXX_test
check_PROGRAMS          += CXX_test
CXX_test_SOURCES         = CXX_test.cpp

TESTS                   += F90_test
check_PROGRAMS          += F90_test
F90_test_SOURCES         = F90_test.f90

TESTS                   += C_openmp_test
check_PROGRAMS          += C_openmp_test
C_openmp_test_SOURCES    = C_openmp_test.c

TESTS                   += CXX_openmp_test
check_PROGRAMS          += CXX_openmp_test
CXX_openmp_test_SOURCES  = CXX_openmp_test.cpp

TESTS                   += F90_openmp_test
check_PROGRAMS          += F90_openmp_test
F90_openmp_test_SOURCES  = F90_openmp_test.f90

TESTS            	+= man_pages
TESTS            	+= debugger
TESTS            	+= version_match
TESTS            	+= rm_execution
