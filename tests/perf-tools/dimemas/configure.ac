#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ([2.71])
AC_INIT([dimemas],[5.3.4],[https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary extrae environment variables set


AC_MSG_CHECKING([for DIMEMAS_INC environment variable])
if test "x$DIMEMAS_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR(DIMEMAS_INC not defined - please load dimemas environment.)
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

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
echo '-------------------------------------------------------------------------------------------------------------'
