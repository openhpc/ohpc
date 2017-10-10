#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(geopm, 0.4.0, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for GEOPM_DIR environment variable])
if test "x$GEOPM_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([GEOPM_DIR not defined - please load geopm environment.])
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
AC_PROG_CXX
AC_PROG_F77
AC_PROG_FC

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# OMP_FLAGS: Flags for enabling OpenMP
if test "x$OMP_FLAGS" = "x"; then
    OMP_FLAGS="-fopenmp"
fi

if test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
    CFLAGS="$GEOPM_CFLAGS $OMP_FLAGS -DTUTORIAL_ENABLE_MKL -D_GNU_SOURCE -std=c99 -xAVX $CFLAGS"
    CXXFLAGS="$GEOPM_CFLAGS $OMP_FLAGS -DTUTORIAL_ENABLE_MKL -D_GNU_SOURCE -std=c++11 -xAVX $CXXFLAGS"
    LDFLAGS="$GEOPM_LDFLAGS $OMP_FLAGS -lm -lrt -mkl -xAVX $LDFLAGS"
else
    CFLAGS="$GEOPM_CFLAGS $OMP_FLAGS -std=gnu11 -mavx"
    CXXFLAGS="$GEOPM_CFLAGS $OMP_FLAGS -std=gnu++11 -mavx"
    LDFLAGS="$GEOPM_LDFLAGS $OMP_FLAGS -lm -lrt -mavx"
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
echo F77 compiler ................. : `which $F77`
echo F90 compiler ................. : `which $F77`
echo
echo C compiler flags.............. : $CFLAGS
echo F77 compiler flags............ : $FFLAGS
echo F90 compiler flags............ : $FCFLAGS
echo
echo '-------------------------------------------------------------------------------------------------------------'
