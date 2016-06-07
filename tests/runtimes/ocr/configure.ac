#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([ocr], [2.1], [karl.w.schulz@intel.com])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for OCR_LIB environment variable])
if test "x$OCR_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OCR_LIB not defined - please load ocr environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for OCR_INC environment variable])
if test "x$OCR_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OCR_INC not defined - please load ocr environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I${OCR_INC} ${CFLAGS}"
LDFLAGS="-L${OCR_LIB} -locr_${OCR_TYPE} -lpthread -lm ${LDFLAGS}"
CPPFLAGS="-I.."

if test "$LMOD_FAMILY_COMPILER" = "intel"; then
   CFLAGS="${CFLAGS} -fp-model strict"
fi

AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu")
AM_COND_IF(USE_GNU_COMPILER, LDFLAGS="${LDFLAGS} ")

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/ohpc_module/Makefile)

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
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
