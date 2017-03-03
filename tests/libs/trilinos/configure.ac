#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(trilinos, 12.4.2, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for TRILINOS_DIR environment variable])
if test "x$TRILINOS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([TRILINOS_DIR not defined - please load trilinos environment.])
else
   AC_MSG_RESULT([yes])
fi

if test "x$LMOD_FAMILY_COMPILER" = "xgnu"; then

AC_MSG_CHECKING([for OPENBLAS environment variable])
if test "x$OPENBLAS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OPENBLAS_DIR not defined - please load openblas environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for Parallel HDF5 environment variable])
if test "x$HDF5_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HDF5_DIR not defined - please load phdf5 environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for NETCDF environment variable])
if test "x$NETCDF_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([NETCDF_DIR not defined - please load netcdf environment.])
else
   AC_MSG_RESULT([yes])
fi

fi

# set compilers to use MPI toolchain 
CC=mpicc
CXX=mpicxx
FC=mpif90

# test compilers
AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST


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
echo Fortran compiler ............. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
