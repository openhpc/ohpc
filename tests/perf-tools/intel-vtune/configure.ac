AC_PREREQ([2.63])
AC_INIT([vtune-tests], [0.10.0], [karl.w.schulz@intel.com])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have module loaded 

AC_MSG_CHECKING([for VTUNE_DIR environment variable])
if test "x$VTUNE_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([VTUNE_DIR not defined - please load vtune environment.])
else
   AC_MSG_RESULT([yes])
fi

# set compilers to use MPI toolchain 

CC=mpicc

# test compilers

AC_PROG_CC

AC_OUTPUT( Makefile tests/Makefile)

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............. : $LMOD_FAMILY_MPI
echo
echo C compiler.................... : `which $CC`
echo C compiler flags.............. : $CFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
