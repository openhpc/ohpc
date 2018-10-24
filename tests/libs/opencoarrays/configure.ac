#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(opencoarrays, 2.2.0, [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for OPENCOARRAYS_DIR environment variable])
if test "x$OPENCOARRAYS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([OPENCOARRAYS_DIR not defined - please load opencoarrays environment.])
else
   AC_MSG_RESULT([no])
fi

# set compilers to use MPI toolchain 
F77=mpif77
FC=mpif90

# test compilers
AC_PROG_F77
AC_PROG_FC

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

FFLAGS="-fcoarray=lib -I ${OPENCOARRAYS_INC} ${FFLAGS}"
FCFLAGS="-fcoarray=lib -I ${OPENCOARRAYS_INC} ${FCFLAGS}"
LDFLAGS="-L${OPENCOARRAYS_LIB} -lcaf_mpi ${LDFLAGS}"


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
echo F77 compiler ................. : `which $F77`
echo F90 compiler ................. : `which $FC`
echo 
echo F77 compiler flags............ : $FFLAGS
echo F90 compiler flags............ : $FCFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
