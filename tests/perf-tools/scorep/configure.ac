#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(scorep, 8.4, https://github.com/openhpc/ohpc)
AC_CONFIG_MACRO_DIR([m4])
AC_CONFIG_AUX_DIR([.])
AM_INIT_AUTOMAKE([foreign subdir-objects])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AC_SUBST([LOG_DRIVER],['$(SHELL) $(top_srcdir)/../../test-driver-ohpc'])

# verify we have necessary scorep environment variables set
AC_MSG_CHECKING([for SCOREP_DIR environment variable])
if test "x$SCOREP_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCOREP_DIR not defined - please load scorep environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_CONFIG_SUBDIRS([tests/mpi])
AC_CONFIG_SUBDIRS([tests/serial])
AC_CONFIG_FILES(Makefile tests/Makefile)
AC_OUTPUT()
