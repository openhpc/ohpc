#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([boost-mpi], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

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

CXX=mpicxx
AC_PROG_CXX

# Require boost and variety of libraries

#AC_DEFINE([MPICXX],[mpicxx])

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/module/Makefile tests/mpi/test/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo
echo C++ compiler.................. : `which $CXX`
echo C++ compiler flags............ : $CXXFLAGS
echo Boost CPPFLAGS flags.......... : -I$BOOST_DIR/include
echo 
echo '-------------------------------------------------------------------------------------------------------------'
