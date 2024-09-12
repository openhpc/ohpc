#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([gsl], [2.1], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for GSL_LIB environment variable])
if test "x$GSL_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([GSL_LIB not defined - please load gsl environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for GSL_INC environment variable])
if test "x$GSL_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([GSL_INC not defined - please load gsl environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_CXX

# Define include path and library linkage from environment variables
# which should be provided via module loads.

CFLAGS="-I${GSL_INC} ${CFLAGS}"
CXXFLAGS="-I${GSL_INC} ${CXXFLAGS} -fopenmp"
LDFLAGS="-L${GSL_LIB} -lgsl -lgslcblas ${LDFLAGS}"
CPPFLAGS="-I.."

if test "$LMOD_FAMILY_COMPILER" = "intel"; then
   CFLAGS="${CFLAGS} -fp-model strict"
fi

AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu")
AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu12")
AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu13")
AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu14")
AM_COND_IF(USE_GNU_COMPILER, LDFLAGS="${LDFLAGS} -lm")

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/ohpc_module/Makefile tests/err/Makefile \
tests/sys/Makefile tests/const/Makefile tests/min/Makefile \
tests/multimin/Makefile tests/ieee-utils/Makefile tests/fft/Makefile \
tests/specfunc/Makefile tests/dht/Makefile tests/fit/Makefile \
tests/multifit/Makefile tests/bspline/Makefile tests/statistics/Makefile \
tests/sum/Makefile tests/roots/Makefile tests/multiroots/Makefile \
tests/ntuple/Makefile tests/poly/Makefile tests/qrng/Makefile \
tests/rng/Makefile tests/randist/Makefile tests/siman/Makefile \
tests/integration/Makefile tests/interpolation/Makefile tests/block/Makefile \
tests/vector/Makefile tests/matrix/Makefile tests/histogram/Makefile \
tests/monte/Makefile tests/ode-initval/Makefile tests/ode-initval2/Makefile \
tests/cblas/Makefile tests/linalg/Makefile tests/eigen/Makefile \
tests/permutation/Makefile tests/combination/Makefile tests/multiset/Makefile \
tests/sort/Makefile tests/complex/Makefile tests/diff/Makefile \
tests/deriv/Makefile tests/cheb/Makefile tests/cdf/Makefile \
tests/wavelet/Makefile tests/multilarge/Makefile tests/rstat/Makefile \
tests/spblas/Makefile tests/splinalg/Makefile tests/spmatrix/Makefile \
tests/test_gsl_histogram/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo
echo C compiler.................... : `which $CC`
echo C++ compiler.................. : `which $CXX`
echo
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo
echo '-------------------------------------------------------------------------------------------------------------'
