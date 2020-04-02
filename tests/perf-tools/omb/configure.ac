#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(omb, 5.6.1, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for OMB_DIR environment variable])
if test "x$OMB_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OMB_DIR not defined - please load omb environment.])
else
   AC_MSG_RESULT([no])
fi

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version................ : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo 
echo '-------------------------------------------------------------------------------------------------------------'
