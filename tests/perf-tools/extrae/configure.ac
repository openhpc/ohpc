#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(extrae, 3.5.2, https://github.com/openhpc/ohpc)
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary extrae environment variables set

AC_MSG_CHECKING([for EXTRAE_LIB environment variable])
if test "x$EXTRAE_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([EXTRAE_LIB not defined - please load extrae environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for EXTRAE_INC environment variable])
if test "x$EXTRAE_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([EXTRAE_INC not defined - please load extrae environment.])
else
   AC_MSG_RESULT([yes])
fi


OHPC_COMPILER_FAMILY

CC="mpicc"
CXX="mpicxx"
FC="mpif90"
F77="mpif77"


# test compilers

AC_PROG_CC
AC_PROG_F77

CFLAGS="-I${EXTRAE_INC} ${CFLAGS}"
CXXFLAGS="-I${EXTRAE_INC} ${CXXFLAGS}"
FCFLAGS="-I${EXTRAE_INC} ${FCLAGS}"
FFLAGS="-I${EXTRAE_INC} ${FCLAGS}"
LDFLAGS="-L${EXTRAE_LIB} ${LDFLAGS}"

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
echo C++ compiler flags............ : $CXXFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo LDFLAGS linker flags.......... : $LDFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
