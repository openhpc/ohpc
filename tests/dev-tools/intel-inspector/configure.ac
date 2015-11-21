#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([inspector], [0.10.0], [karl.w.schulz@intel.com])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for INSPECTOR_DIR environment variable])
if test "x$INSPECTOR_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([INSPECTOR_DIR not defined - please load inspector environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "
FC=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_FC

AC_OUTPUT( Makefile tests/Makefile)

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo
echo Fortran compiler.............. : `which $FC`
echo 
echo '-------------------------------------------------------------------------------------------------------------'
