#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([adios], [1.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for ADIOS_DIR environment variable])
if test "x$ADIOS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([ADIOS_DIR not defined - please load adios environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for HDF5_DIR environment variable])
if test "x$HDF5_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HDF5_DIR not defined - please load phdf5 environment.])
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

# test compilers
CC=mpicc
FC=mpif90

AC_PROG_CC
AC_PROG_FC

CFLAGS="-I${ADIOS_INC} ${CFLAGS}"
FCFLAGS="-I${ADIOS_INC} ${FCFLAGS}"
LDFLAGS="-L${ADIOS_LIB} -L${HDF5_LIB}"
LIBS="-ladios -lm -lz -lpthread -lhdf5"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile )

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
echo C compiler flags.............. : $CFLAGS
echo F90 compiler.................. : `which $FC`
echo F90 compiler flags............ : $FCFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
