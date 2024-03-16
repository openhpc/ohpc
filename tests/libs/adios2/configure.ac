#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([adios2], [2.8.3], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for ADIOS2_DIR environment variable])
if test "x$ADIOS2_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([ADIOS2_DIR not defined - please load adios2 environment.])
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
CXX=mpic++
FC=mpif90

AC_PROG_CXX
AC_PROG_FC

CXXFLAGS="-DADIOS2_USE_MPI -I${ADIOS2_INC} ${CXXFLAGS}"
FCFLAGS="-DADIOS2_USE_MPI -I${ADIOS2_INC}/adios2/fortran ${FCFLAGS}"
LDFLAGS="-L${HDF5_LIB} -L${ADIOS2_LIB}"
LIBS="-lm -lhdf5"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version................. : $PACKAGE-$VERSION
echo OHPC compiler toolchain......... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain.............. : $LMOD_FAMILY_MPI
echo
echo CXX compiler.................... : `which $CXX`
echo CXX compiler flags.............. : $CXXFLAGS
echo F90 compiler.................... : `which $FC`
echo F90 compiler flags.............. : $FCFLAGS
echo LD flags........................ : $LDFLAGS
echo LIBS............................ : $LIBS
echo
echo '-------------------------------------------------------------------------------------------------------------'
