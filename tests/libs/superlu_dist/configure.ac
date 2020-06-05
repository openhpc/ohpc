#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(superlu_dist, 4.0, [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for SUPERLU_DIST_DIR environment variable])
if test "x$SUPERLU_DIST_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SUPERLU_DIST_DIR not defined - please load superlu_dist environment.])
else
   AC_MSG_RESULT([no])
fi

OHPC_COMPILER_FAMILY

# set compilers to use MPI toolchain 
CC=mpicc
CXX=mpicxx
FC=mpif90

# test compilers
AC_PROG_CC
AC_PROG_FC
AC_PROG_RANLIB

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

CFLAGS="-I${SUPERLU_DIST_INC} ${CFLAGS}"
FFLAGS="-I${SUPERLU_DIST_INC} ${FFLAGS}"
LDFLAGS="-L${SUPERLU_DIST_LIB} -lsuperlu_dist ${OHPC_BLAS} ${LDFLAGS}"

# check if using OpenMPI
AC_MSG_CHECKING([if using OpenMPI variant])
if ( sh -c "echo $LMOD_FAMILY_MPI" | grep -q openmpi ); then
   AC_MSG_RESULT([yes])
   LDFLAGS="${LDFLAGS} -lmpi_cxx"
else
   AC_MSG_RESULT([no])
fi


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
echo Fortran compiler ............. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo Fortran compiler flags........ : $FFLAGS
echo
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
