#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([oom], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# unset default compilers and detect compiler toolchain from environment
CC=" "
CXX=" "

OHPC_COMPILER_FAMILY

# test compilers
AC_PROG_CC
AC_PROG_FC

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
