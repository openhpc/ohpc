#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([R_base], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for R_DIR environment variable])
if test "x$R_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([R_DIR not defined - please load R_base environment.])
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

AC_PROG_CXX

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo
echo R location.................... : `which $R_BIN/R`
echo Rscript location.............. : `which $R_BIN/Rscript`
echo R_base install dir............ : $R_DIR
echo R_base lib dir................ : $R_LIB
echo R_base share dir.............. : $R_SHARE
echo 
echo '-------------------------------------------------------------------------------------------------------------'
