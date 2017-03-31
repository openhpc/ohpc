#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([netcdf], [0.10.0], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set

CHECK_ENV([NETCDF_LIB],[netcdf])
CHECK_ENV([NETCDF_INC],[netcdf])
CHECK_ENV([NETCDF_CXX_LIB],[netcdf-cxx])
CHECK_ENV([NETCDF_CXX_INC],[netcdf-cxx])
CHECK_ENV([NETCDF_FORTRAN_LIB],[netcdf-fortran])
CHECK_ENV([NETCDF_FORTRAN_INC],[netcdf-fortran])
CHECK_ENV([HDF5_LIB],[hdf5])

OHPC_COMPILER_FAMILY

# Define include path and library linkage from environment variables
# which should be provided via module loads. 
CFLAGS="-I${NETCDF_INC} ${CFLAGS}"
CXXFLAGS="-I${NETCDF_INC} -I${NETCDF_CXX_INC} ${CXXFLAGS}"
FCFLAGS="-I${NETCDF_FORTRAN_INC} ${FCFLAGS}"

#C_LDFLAGS="-L$NETCDF_LIB -L$HDF5_LIB -lnetcdf -lhdf5 -lm"
CXX_LDFLAGS="-L$NETCDF_CXX_LIB -lnetcdf_c++4"
FC_LDFLAGS="-L$NETCDF_FORTRAN_LIB -lnetcdff"
LDFLAGS="-L$NETCDF_LIB -L$HDF5_LIB"
LIBS="-lnetcdf -lhdf5 $LIBS"

AC_SUBST(C_LDFLAGS)
AC_SUBST(CXX_LDFLAGS)
AC_SUBST(FC_LDFLAGS)

# test compilers

AC_PROG_CC
AC_PROG_CXX
AC_PROG_FC

# Test if math library is enabled by default
AC_LANG([C])
AC_SEARCH_LIBS([cos],[m],[],[AC_MSG_ERROR([no cos available])])

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests-parallel/Makefile)

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
echo 
echo C linker flags................ : $C_LDFLAGS
echo C++ linker flags.............. : $CXX_LDFLAGS
echo Fortran linker flags.......... : $FC_LDFLAGS
echo Linker flags.................. : $LDFLAGS $LIBS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
