#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([boost], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for BOOST_DIR environment variable])
if test "x$BOOST_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([BOOST_DIR not defined - please load boost environment.])
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

# test compilers

# (TRon Mar 25, 2015)
# Added the AM conditionals below for use with mkl-ublas header
# with different options for ICC and GCC
AM_CONDITIONAL(ICC, test $LMOD_FAMILY_COMPILER = "intel")
AM_CONDITIONAL(GCC, test $LMOD_FAMILY_COMPILER = "gnu")

AC_PROG_CXX

# AC_PROG_CXX's own built-in C++ standard detection only probes up
# through C++11, embedding "-std=gnu++11" into CXXFLAGS; Boost 1.92's
# Boost.Math requires C++14. Append a newer -std= after it on the
# same compile line, which wins since gcc honors the last one given;
# same fix already applied in mfem's configure.ac.
CXXFLAGS="${CXXFLAGS} -std=c++17"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile \
                         tests/module/Makefile \
                         tests/program_options/test/Makefile \
                         tests/regex/test/Makefile \
                         tests/regex/test/regress/Makefile \
                         tests/multi_array/test/Makefile \
                         tests/accumulators/test/Makefile \
                         tests/random/test/Makefile \
                         tests/numeric/test/Makefile \
                         )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............ : $LMOD_FAMILY_MPI
echo
echo C++ compiler.................. : `which $CXX`
echo C++ compiler flags............ : $CXXFLAGS
echo Boost CPPFLAGS flags.......... : -I$BOOST_DIR/include
echo
echo '-------------------------------------------------------------------------------------------------------------'
