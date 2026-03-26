# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ([2.71])
AC_INIT([numpy],[0.10.0],[https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for NUMPY_DIR environment variable])
if test "x$NUMPY_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR(NUMPY_DIR not defined - please load numpy environment.)
else
   AC_MSG_RESULT([yes])
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
echo
echo Numpy install dir............. : $NUMPY_DIR
echo
echo '-------------------------------------------------------------------------------------------------------------'
