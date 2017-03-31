#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([mpiP], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for MPI_LIB environment variable])
if test "x$MPIP_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([MPIP_LIB not defined - please load mpiP environment.])
else
   AC_MSG_RESULT([yes])
fi


CXX=mpicxx
CC=mpicc
FC=mpif90

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

#LDFLAGS="-L${MPIP_LIB} -lmpiP -lbfd -liberty -lm ${LDFLAGS}"

# test compilers

AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC

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
echo C++ compiler.................. : `which $CXX`
echo F90 compiler.................. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
