TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
AUTOMAKE_OPTIONS  = no-dependencies

# Pure OpenMP tests
check_PROGRAMS         = main_omp_c
main_omp_c_CFLAGS      = $(CFLAGS)
main_omp_c_LDFLAGS     = $(LDFLAGS)
main_omp_c_SOURCES     = src/OpenMP/C/main.c

check_PROGRAMS        += main_omp_cxx
main_omp_cxx_CXXFLAGS  = $(CXXFLAGS)
main_omp_cxx_LDFLAGS   = $(LDFLAGS)
main_omp_cxx_SOURCES   = src/OpenMP/C++/main.cpp

check_PROGRAMS        += main_omp_fort
main_omp_fort_FCFLAGS  = $(FCFLAGS)
main_omp_fort_LDFLAGS  = $(LDFLAGS)
main_omp_fort_SOURCES  = src/OpenMP/Fortran/main.F90
#----------------------------------------#

CLEANFILES = *.input.*
