#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([metis], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for METIS_LIB environment variable])
if test "x$METIS_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([METIS_LIB not defined - please load metis environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for METIS_INC environment variable])
if test "x$METIS_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([METIS_INC not defined - please load metis environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment
CC=" "
CXX=" "

OHPC_COMPILER_FAMILY

# Define include path and library linkage from environment variables
# which should be provided via module loads. 
CFLAGS="-I${METIS_INC} ${CFLAGS}"
LDFLAGS="-L${METIS_LIB} -lmetis ${LDFLAGS}"

# test compilers
AC_PROG_CC

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo
echo C compiler.................... : `which $CC`
echo C compiler flags.............. : $CFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
