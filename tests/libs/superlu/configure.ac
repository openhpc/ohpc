#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(superlu, 4.3, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for SUPERLU_DIR environment variable])
if test "x$SUPERLU_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SUPERLU_DIR not defined - please load superlu environment.])
else
   AC_MSG_RESULT([no])
fi

#OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_F77
AC_PROG_CXX
# Add the suffix to CC for shared linking.  Can't just set as an option
# because it must be first.
dnl if test "$shared_suffix" && test ! `echo $CC | grep "$shared_suffix"`; then
dnl     CC=${CC}${shared_suffix}
dnl fi
# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Define include path and library linkage from environment variables
# which should be provided via module loads. 
if test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
CFLAGS="-I${SUPERLU_INC} ${CFLAGS} -I ${MKLROOT}/include"
FFLAGS="-I${SUPERLU_INC} ${FFLAGS} -I ${MKLROOT}/include"
LDFLAGS="-L${SUPERLU_LIB} -lsuperlu -L$MKLROOT/lib/intel64 -lmkl_rt -lm ${LDFLAGS}"
else
CFLAGS="-I${SUPERLU_INC} ${CFLAGS}"
FFLAGS="-I${SUPERLU_INC} ${FFLAGS}"
LDFLAGS="-L${SUPERLU_LIB} -lsuperlu -L${OPENBLAS_LIB} -lopenblas ${LDFLAGS}"
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
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo C++ compiler.................. : `which $CXX`
echo Fortran compiler ............. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
