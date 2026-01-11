AC_PREREQ([2.71])
AC_INIT([compiler-tests],[0.0.1],[https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../test-driver-ohpc'])

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "
FC=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX

if test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
  CFLAGS="${CFLAGS} -qopenmp"
  CXXFLAGS="${CXXFLAGS} -qopenmp"
  FCFLAGS="${FCFLAGS} -qopenmp"
else
  CFLAGS="${CFLAGS} -fopenmp"
  CXXFLAGS="${CXXFLAGS} -fopenmp"
  FCFLAGS="${FCFLAGS} -fopenmp"
fi

AC_CONFIG_FILES([Makefile tests/Makefile])
AC_OUTPUT

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
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
