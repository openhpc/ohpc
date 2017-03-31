#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(fftw, 0.10.0, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary fftw environment variables set

AC_MSG_CHECKING([for FFTW_LIB environment variable])
if test "x$FFTW_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([FFTW_LIB not defined - please load fftw environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for FFTW_INC environment variable])
if test "x$FFTW_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([FFTW_INC not defined - please load fftw environment.])
else
   AC_MSG_RESULT([yes])
fi

# set compilers to use MPI toolchain 

CC=mpicc
CXX=mpicxx
FC=mpif90

#OHPC_COMPILER_FAMILY

# test compilers

AC_PROG_CC
AC_PROG_FC
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

CFLAGS="-I ${FFTW_INC} ${CFLAGS}"
CXXFLAGS="-I ${FFTW_INC} ${CXXFLAGS} -fopenmp"
FCFLAGS="-I ${FFTW_INC} ${FCLAGS}"
LDFLAGS="-L${FFTW_LIB} ${LDFLAGS}"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/ohpc_module/Makefile)

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
echo Fortran compiler ............. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
