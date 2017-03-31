#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([papi], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for PAPI_LIB environment variable])
if test "x$PAPI_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PAPI_LIB not defined - please load papi environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for PAPI_INC environment variable])
if test "x$PAPI_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PAPI_INC not defined - please load papi environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "

OHPC_COMPILER_FAMILY

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I ${PAPI_INC} ${CFLAGS}"
FFLAGS="-g -I ${PAPI_INC} ${FCFLAGS}"
LDFLAGS="-L${PAPI_LIB} -lpapi ${LDFLAGS}"

# test compilers

AC_PROG_CC
AC_PROG_F77
AC_PROG_RANLIB

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
echo F77 compiler.................. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo F77 compiler flags............ : $FFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
