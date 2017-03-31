AC_PREREQ([2.63])
AC_INIT([compiler-tests], [0.0.1], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "
FC=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX

CFLAGS="${CFLAGS} -fopenmp"
CXXFLAGS="${CXXFLAGS} -fopenmp"
FCFLAGS="${FCFLAGS} -fopenmp"

AC_OUTPUT( Makefile tests/Makefile)

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
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
