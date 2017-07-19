#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(slepc, 3.7.4, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for SLEPC_DIR environment variable])
if test "x$SLEPC_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SLEPC_DIR not defined - please load slepc environment.])
else
   AC_MSG_RESULT([no])
fi

# set compilers to use MPI toolchain 
CC=mpicc
CXX=mpicxx
F77=mpif77
FC=mpif90

# test compilers
AC_PROG_CC
AC_PROG_F77
AC_PROG_FC
AC_PROG_CXX

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

if test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
  CFLAGS="-I ${SLEPC_INC} -I ${MKLROOT}/include ${CFLAGS}"
  CXXFLAGS="-I ${SLEPC_INC} -I ${MKLROOT}/include ${CXXFLAGS} -fopenmp"
  FFLAGS="-I ${SLEPC_INC} -I ${MKLROOT}/include ${FFLAGS}"
  LDFLAGS="-L${SLEPC_LIB} -lSLEPC ${LDFLAGS}"
else
  CFLAGS="-I ${SLEPC_INC} ${CFLAGS}"
  CXXFLAGS="-I ${SLEPC_INC} ${CXXFLAGS} -fopenmp"
  FFLAGS="-I ${SLEPC_INC} ${FFLAGS}"
  LDFLAGS="-L${SLEPC_LIB} -lSLEPC ${LDFLAGS}"
fi


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
echo Fortran compiler ............. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
