#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([petsc], 0.10.0, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_MACRO_DIR([m4])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary petsc environment variables set

CHECK_ENV([PETSC_LIB],[petsc])
CHECK_ENV([PETSC_INC],[petsc])
CHECK_ENV([HDF5_INC],[hdf5])

# set compilers to use MPI toolchain 

CC=mpicc
CXX=mpicxx
F77=mpif77
FC=mpif90

# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_F77
AC_PROG_CXX

# Test if math library is enabled by default
AC_LANG([C])
AC_SEARCH_LIBS([cos],[m],[],[AC_MSG_ERROR([no cos available])])

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I${PETSC_INC} ${CFLAGS}"
FFLAGS="-I${PETSC_INC} ${FFLAGS}"
FCFLAGS="-I${PETSC_INC} ${FCFLAGS}"
LDFLAGS="-L${PETSC_LIB}"
LIBS="-lpetsc $LIBS"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo Fortran compiler ............. : `which $FC`
echo F77 compiler ................. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo F77 compiler flags............ : $FFLAGS
echo Linker flags.................. : $LDFLAGS $LIBS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
