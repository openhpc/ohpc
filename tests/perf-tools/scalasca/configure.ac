#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ([2.69])
AC_INIT([scalasca],[2.6.2],[https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary scalasca environment variables set

AC_MSG_CHECKING([for SCALASCA_DIR environment variable])
if test "x$SCALASCA_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR([SCALASCA_DIR not defined - please load scalasca environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_CONFIG_SUBDIRS([tests/mpi])
AC_CONFIG_SUBDIRS([tests/serial])
AC_CONFIG_FILES(Makefile tests/Makefile)
AC_OUTPUT
