#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(scotch, 6.0.4, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for SCOTCH_DIR environment variable])
if test "x$SCOTCH_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCOTCH_DIR not defined - please load scotch environment.])
else
   AC_MSG_RESULT([no])
fi

# test compilers
AC_PROG_CC

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

CFLAGS="-I ${SCOTCH_INC} ${CFLAGS}"
LDFLAGS="-L${SCOTCH_LIB} -lscotch  -lscotcherrexit -lz -lbz2 -lm -lrt -pthread ${LDFLAGS}"


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
echo 
echo C compiler flags.............. : $CFLAGS
echo Linker flags.................. : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
