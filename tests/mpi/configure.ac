AC_PREREQ([2.63])
AC_INIT([mpi-tests], [0.10.0], [https://github.com/openhpc/ohpc])
AM_INIT_AUTOMAKE
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

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
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain............ : $LMOD_FAMILY_MPI
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
