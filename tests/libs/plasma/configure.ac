#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([plasma], [2.1], [takeharu.kato@linaro.org])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for PLASMA_LIB environment variable])
if test "x$PLASMA_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PLASMA_LIB not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for PLASMA_INC environment variable])
if test "x$PLASMA_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PLASMA_INC not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for OPENBLAS_INC environment variable])
if test "x$OPENBLAS_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OPENBLAS_INC not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for OPENBLAS_LIB environment variable])
if test "x$OPENBLAS_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OPENBLAS_LIB not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for SCALAPACK_LIB environment variable])
if test "x$OPENBLAS_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCALAPACK_LIB not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "

OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC
AC_PROG_F77

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I${OPENBLAS_INC} -I${PLASMA_INC} ${CFLAGS}"
CXXFLAGS="-I${PLASMA_INC} ${CXXFLAGS} -fopenmp"
FFLAGS="-I${PLASMA_INC} ${FFLAGS}"
LDFLAGS="-L${OPENBLAS_LIB} -L${PLASMA_LIB} -L${SCALAPACK_LIB} -lplasma -lcoreblasqw -lcoreblas -lquark -llapacke -lscalapack -lopenblas -lpthread -lm ${LDFLAGS}"
CPPFLAGS="-I${OPENBLAS_INC} -I${PLASMA_INC} ${CFLAGS}"

if test "$LMOD_FAMILY_COMPILER" = "intel"; then
   CFLAGS="${CFLAGS} -fp-model strict"
fi

AM_CONDITIONAL(USE_GNU_COMPILER, test "$LMOD_FAMILY_COMPILER" = "gnu")
AM_COND_IF(USE_GNU_COMPILER, LDFLAGS="${LDFLAGS} -lm")

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/lin/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo
echo C compiler.................... : `which $CC`
echo C++ compiler.................. : `which $CXX`
echo Fortran compiler.................. : `which $F77`

echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags ....... : $FFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
