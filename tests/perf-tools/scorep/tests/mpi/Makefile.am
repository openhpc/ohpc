TESTS_ENVIRONMENT = BATS_NO_SUMMARY=1
AUTOMAKE_OPTIONS  = no-dependencies

# Pure MPI tests
check_PROGRAMS         = main_mpi_c
main_mpi_c_CFLAGS      = $(CFLAGS)
main_mpi_c_LDFLAGS     = $(LDFLAGS)
main_mpi_c_SOURCES     = src/MPI/C/main.c

check_PROGRAMS        += main_mpi_cxx
main_mpi_cxx_CXXFLAGS  = $(CXXFLAGS)
main_mpi_cxx_LDFLAGS   = $(LDFLAGS)
main_mpi_cxx_SOURCES   = src/MPI/C++/main.cpp

check_PROGRAMS        += main_mpi_fort
main_mpi_fort_FCFLAGS  = $(FCFLAGS)
main_mpi_fort_LDFLAGS  = $(LDFLAGS)
main_mpi_fort_SOURCES  = src/MPI/Fortran/main.F90

#----------------------------------------#

# MPI + OpenMP tests
check_PROGRAMS        += main_hyb_c
main_hyb_c_CFLAGS      = $(CFLAGS)
main_hyb_c_LDFLAGS     = $(LDFLAGS)
main_hyb_c_SOURCES     = src/hybrid/C/main.c

check_PROGRAMS        += main_hyb_cxx
main_hyb_cxx_CXXFLAGS  = $(CXXFLAGS)
main_hyb_cxx_LDFLAGS   = $(LDFLAGS)
main_hyb_cxx_SOURCES   = src/hybrid/C++/main.cpp

check_PROGRAMS        += main_hyb_fort
main_hyb_fort_FCFLAGS  = $(FCFLAGS)
main_hyb_fort_LDFLAGS  = $(LDFLAGS)
main_hyb_fort_SOURCES  = src/hybrid/Fortran/main.F90

#----------------------------------------#

CLEANFILES = *.input.*
