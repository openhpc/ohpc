#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(mumps, 4.0.2, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

AC_MSG_CHECKING([for MUMPS_DIR environment variable])
if test "x$MUMPS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([MUMPS_DIR not defined - please load mumps environment.])
else
   AC_MSG_RESULT([no])
fi

# verify MUMPS Makefile.inc is available
AC_CHECK_FILE([$MUMPS_DIR/etc/Makefile.inc],[],[AC_ERROR[MUMPS_DIR/etc/Makefile.inc not available]])
SCALAPACK=`grep "^SCALAP" $MUMPS_DIR/etc/Makefile.inc | awk '{$1=$2="";print $0}'`
AC_SUBST(SCALAPACK)

# set compilers to use MPI toolchain 

CC=mpicc
CXX=mpicxx
F77=mpif90
FC=mpif90

# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_F77

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I${MUMPS_INC} ${CFLAGS}"
FFLAGS="-I${MUMPS_INC} ${FFLAGS}"
LDFLAGS="-L${MUMPS_LIB}"
LIBS="-lmumps_common"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile)

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............ : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo Fortran compiler ............. : `which $F77`
echo 
echo C compiler flags.............. : $CFLAGS
echo Fortran compiler flags........ : $FFLAGS
echo SCALAPACK linkage............. : $SCALAPACK
echo LDFLAGS linker flags.......... : $LDFLAGS
echo LIBS ......................... : $LIBS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
