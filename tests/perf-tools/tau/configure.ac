#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ([2.71])
AC_INIT([tau],[0.10.0],[https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary tau environment variables set

AC_MSG_CHECKING([for TAU_LIB environment variable])
if test "x$TAU_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR(TAU_LIB not defined - please load tau environment.)
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for TAU_INC environment variable])
if test "x$TAU_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR(TAU_INC not defined - please load tau environment.)
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for TAU_MAKEFILE environment variable])
if test "x$TAU_MAKEFILE" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR(TAU_MAKEFILE not defined - please load tau environment.)
else
   AC_MSG_RESULT([yes])
fi

# set compilers to use Tau wrappers

OHPC_COMPILER_FAMILY
OHPC_DISTRO_FAMILY

CC=tau_cc.sh
CXX=tau_cxx.sh
FC=tau_f90.sh
F77=tau_f77.sh


# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX
AC_PROG_F77
# Add the suffix to CC for shared linking.  Can't just set as an option
# because it must be first.
dnl if test "$shared_suffix" && test ! `echo $CC | grep "$shared_suffix"`; then
dnl     CC=${CC}${shared_suffix}
dnl fi
# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Define include path and library linkage from environment variables
# which should be provided via module loads.

CFLAGS="-I${TAU_INC} ${CFLAGS}"
CXXFLAGS="-I${TAU_INC} ${CXXFLAGS}"
FCFLAGS="-I${TAU_INC} ${FCLAGS}"
FFLAGS="-I${TAU_INC} ${FCLAGS}"
LDFLAGS="-L${TAU_LIB} -lTAU ${LDFLAGS}"

if test "x$LMOD_FAMILY_COMPILER" = "xintel" ; then
   if test "x$LMOD_FAMILY_MPI" = "xmpich" ;then
      LDFLAGS="$LDFLAGS -lfmpich"
   fi
   if test "x$LMOD_FAMILY_MPI" = "xmvapich2" ;then
      LDFLAGS="$LDFLAGS -lfmpich"
   fi
   CFLAGS="${CFLAGS} -qopenmp"
   CXXFLAGS="${CXXFLAGS} -qopenmp"
else
   CFLAGS="${CFLAGS} -fopenmp"
   CXXFLAGS="${CXXFLAGS} -fopenmp"
fi

if test "x$DISTRO_FAMILY" = "xSLES" ; then
   LDFLAGS="$LDFLAGS -lsframe"
fi

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT

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
