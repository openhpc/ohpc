AC_PREREQ([2.63])
AC_INIT([rms-tests], [0.10.0], [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

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
