#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(scalasca, 2.3.1, karl.w.schulz@intel.com)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary scalasca environment variables set

AC_MSG_CHECKING([for SCALASCA_LIB environment variable])
if test "x$SCALASCA_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCALASCA_LIB not defined - please load scalasca environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for SCALASCA_INC environment variable])
if test "x$SCALASCA_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SCALASCA_INC not defined - please load scalasca environment.])
else
   AC_MSG_RESULT([yes])
fi

# set compilers to use MPI toolchain 

OHPC_COMPILER_FAMILY

CC=mpicc
CXX=mpicxx
FC=mpif90
F77=mpif90



# test compilers

AC_PROG_CC
AC_PROG_FC
AC_PROG_CXX
AC_PROG_F77
# Add the suffix to CC for shared linking.  Can't just set as an option
# because it must be first.
dnl if test "$shared_suffix" && test ! `echo $CC | grep "$shared_suffix"`; then
dnl     CC=${CC}${shared_suffix}
dnl fi
# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Define include path and library linkage from environment variables
# which should be provided via module loads. 

CFLAGS="-I${SCALASCA_INC} ${CFLAGS} -fopenmp"
CXXFLAGS="-I${SCALASCA_INC} ${CXXFLAGS} -fopenmp"
FCFLAGS="-I${SCALASCA_INC} ${FCLAGS}"
FFLAGS="-I${SCALASCA_INC} ${FCLAGS}"
LDFLAGS="-L${SCALASCA_LIB} -lscalasca ${LDFLAGS}"

if test "x$LMOD_FAMILY_COMPILER" = "xintel" ; then
   if test "x$LMOD_FAMILY_MPI" = "xmvapich2" ;then
      LDFLAGS="$LDFLAGS -lfmpich"
   fi
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
echo Fortran compiler ............. : `which $FC`
echo 
echo C compiler flags.............. : $CFLAGS
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
