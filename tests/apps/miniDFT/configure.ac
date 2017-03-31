AC_PREREQ([2.63])
AC_INIT([miniDFT], [0.10.0], [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
if test "x$LMOD_FAMILY_COMPILER" = "xgnu"; then

AC_MSG_CHECKING([for FFTW_DIR environment variable])
if test "x$FFTW_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([FFTW_DIR not defined - please load fftw environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for OPENBLAS environment variable])
if test "x$OPENBLAS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OPENBLAS_DIR not defined - please load openblas environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for SCALAPACK environment variable])
if test "x$SCALAPACK_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCALAPACK_DIR not defined - please load scalapack environment.])
else
   AC_MSG_RESULT([yes])
fi

fi

# set compilers to use MPI toolchain 

CC=mpicc
CXX=mpicxx
FC=mpif90

# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX

AC_OUTPUT( Makefile tests/Makefile)

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo C++ compiler.................. : `which $CXX`
echo Fortran compiler ............. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
