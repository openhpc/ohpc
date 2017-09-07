#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT(pnetcdf, 1.8.1, [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

CHECK_ENV([PNETCDF_LIB],[pnetcdf])
CHECK_ENV([PNETCDF_INC],[pnetcdf])

# set compilers to use MPI toolchain 

CC=mpicc
CXX=mpicxx
FC=mpif90

#OHPC_COMPILER_FAMILY

AM_CONDITIONAL([INTEL], [test "x$LMOD_FAMILY_COMPILER" = "xintel"])

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

CFLAGS="-I${PNETCDF_INC} ${CFLAGS}"
CXXFLAGS="-I${PNETCDF_INC} ${CXXFLAGS}"
FCFLAGS="-I${PNETCDF_INC} ${FCLAGS}"
LDFLAGS="-L${PNETCDF_LIB} -lpnetcdf ${LDFLAGS}"

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
