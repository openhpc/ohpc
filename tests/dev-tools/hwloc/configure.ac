# -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.59)
AC_INIT([hwloc], [1.11.6], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for HWLOC_DIR environment variable])
if test "x$HWLOC_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HWLOC_DIR not defined - please load hwloc environment.])
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

# test compilers
AC_PROG_CC

CFLAGS="-I ${HWLOC_INC} ${CFLAGS}"
LDFLAGS="-L${HWLOC_LIB} -lhwloc ${LDFLAGS}"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo
echo hwloc install dir............. : $HWLOC_DIR
echo C compiler flags.............. : $CFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
