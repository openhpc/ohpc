#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(scorep, 8.4, https://github.com/openhpc/ohpc)
AC_CONFIG_MACRO_DIR([../../m4])
AM_INIT_AUTOMAKE([foreign subdir-objects])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# set compilers to use MPI toolchain
OHPC_COMPILER_FAMILY

# test compilers
AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX
AC_PROG_F77
# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Define include path and library linkage from environment variables
# which should be provided via module loads.
CC='scorep-mpicc'
CXX='scorep-mpicxx'
FC='scorep-mpif90'
F77='scorep-mpif90'

if test "x$LMOD_FAMILY_COMPILER" = "xintel" ; then
   CFLAGS="${CFLAGS} -qopenmp -Wno-implicit-function-declaration"
   CXXFLAGS="${CXXFLAGS} -qopenmp -Wno-implicit-function-declaration"
   FCFLAGS="${FCFLAGS} -qopenmp"
   if test "x$LMOD_FAMILY_MPI" = "xmvapich2" ;then
      LDFLAGS="$LDFLAGS -lfmpich"
   fi
else
if test "x$LMOD_FAMILY_COMPILER" = "xgnu14" ; then
  CFLAGS="${CFLAGS} -fopenmp"
  CXXFLAGS="${CXXFLAGS} -fopenmp"
  FCFLAGS="${FCFLAGS} -fopenmp -fallow-argument-mismatch"
else
  CFLAGS="${CFLAGS} -fopenmp"
  CXXFLAGS="${CXXFLAGS} -fopenmp"
  FCFLAGS="${FCLAGS} -fopenmp"
fi
fi
FCFLAGS="${FCFLAGS}"
FFLAGS="${FCFLAGS}"
LDFLAGS="${LDFLAGS}"

AC_CONFIG_FILES([Makefile])

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............ : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo C++ compiler.................. : `which $CXX`
echo Fortran compiler ............. : `which $FC`
echo
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo
echo '-------------------------------------------------------------------------------------------------------------'
