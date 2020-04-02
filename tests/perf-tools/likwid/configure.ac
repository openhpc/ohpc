#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([likwid], [4.3.1], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for LIKWID_LIB environment variable])
if test "x$LIKWID_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([LIKWID_LIB not defined - please load likwid environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for LIKWID_INC environment variable])
if test "x$LIKWID_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([LIKWID_INC not defined - please load likwid environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
FC=" "

OHPC_COMPILER_FAMILY

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-fopenmp -DLIKWID_PERFMON -I ${LIKWID_INC} ${CFLAGS} -lm"
FCFLAGS="-fopenmp -DLIKWID_PERFMON -I ${LIKWID_INC} ${FCFLAGS} -lm"
LDFLAGS="-L${LIKWID_LIB} -llikwid ${LDFLAGS}"

# test compilers

AC_PROG_CC
AC_PROG_FC
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
echo F90 compiler.................. : `which $F90`
echo 
echo C compiler flags.............. : $CFLAGS
echo F90 compiler flags............ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
