#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(plasma, 2.8.0, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for PLASMA_DIR environment variable])
if test "x$PLASMA_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PLASMA_DIR not defined - please load plasma environment.])
else
   AC_MSG_RESULT([yes])
fi
# test compilers
OHPC_COMPILER_FAMILY
AC_PROG_CC
AC_PROG_F77
AC_PROG_FC

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

if test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
  CFLAGS="-I ${PLASMA_INC} -I ${MKLROOT}/include ${CFLAGS}"
  FFLAGS="-I ${PLASMA_INC} -I ${MKLROOT}/include ${FFLAGS}"
  FCFLAGS="-I ${PLASMA_INC} -I ${MKLROOT}/include ${FFLAGS}"
  LDFLAGS="-L${PLASMA_LIB} -lplasma -lcoreblasqw -lcoreblas -lquark -lmkl_rt -lpthread -lifcore ${LDFLAGS}"
else
  CFLAGS="-I ${PLASMA_INC} ${CFLAGS}"
  FFLAGS="-I ${PLASMA_INC} ${FFLAGS}"
  FCFLAGS="-I ${PLASMA_INC} ${FFLAGS}"
  LDFLAGS="-L${PLASMA_LIB} -lplasma -lcoreblasqw -lcoreblas -lquark -L${OPENBLAS_LIB} -lopenblas -lpthread -lm ${LDFLAGS}"
fi


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
echo F77 compiler ................. : `which $F77`
echo F90 compiler ................. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo F77 compiler flags............ : $FFLAGS
echo F90 compiler flags............ : $FCFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
