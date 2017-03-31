#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.

AC_PREREQ(2.59)
AC_INIT(HDF5-tests-env-variables, 0.10, [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
dnl AM_INIT_AUTOMAKE takes a list of options that should be applied to
dnl every Makefile.am when automake is run.
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])
AM_CONFIG_HEADER(config.h)

# karl.w.schulz@intel.com (8/28/14)
# verify we have necessary HDF5 environment variables set

AC_MSG_CHECKING([for HDF5_LIB environment variable])
if test "x$HDF5_LIB" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HDF5_LIB not defined - please load hdf5 environment.])
else
   AC_MSG_RESULT([yes])
fi

AC_MSG_CHECKING([for HDF5_INC environment variable])
if test "x$HDF5_INC" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HDF5_INC not defined - please load hdf5 environment.])
else
   AC_MSG_RESULT([yes])
fi

# unset default compilers and detect compiler toolchain from environment

CC=" "
CXX=" "
FC=" "

OHPC_COMPILER_FAMILY

# Sets compiler.
AC_PROG_CC()
AC_PROG_FC()
AC_PROG_CXX()
# Add the suffix to CC for shared linking.  Can't just set as an option
# because it must be first.
if test "$shared_suffix" && test ! `echo $CC | grep "$shared_suffix"`; then
    CC=${CC}${shared_suffix}
fi
# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Check which versions to build, if no environmental variables are set, build
# all.
if test ! "$H5EX_16" && test ! "$H5EX_18" && test ! "$H5EX_110"
then
  H5EX_16=1
  H5EX_18=1
  H5EX_110=1
fi
AM_CONDITIONAL(V16, test "$H5EX_16")
AM_CONDITIONAL(V18, test "$H5EX_18")
AM_CONDITIONAL(V110, test "$H5EX_110")

if test ! "$H5EX_G" && test ! "$H5EX_D" && test ! "$H5EX_T"
then
  H5EX_G=1
  H5EX_D=1
  H5EX_T=1
fi
AM_CONDITIONAL(H5G, test "$H5EX_G")
AM_CONDITIONAL(H5D, test "$H5EX_D")
AM_CONDITIONAL(H5T, test "$H5EX_T")

# Check if a Fortran 2003 compiler was used for h5fc
HAVE_FORTRAN_2003="no"

AC_MSG_CHECKING([if h5fc was compiled with Fortran 2003 enabled])
if (${FC} -showconfig 2>&1 | grep 'Fortran 2003 Compiler: yes') > /dev/null; then
  HAVE_FORTRAN_2003="yes"	
fi
echo $HAVE_FORTRAN_2003 

# Check if we have Fortran 2003 and Intel compiler; Intel doesn't work with RECURSIVE used by h5ex_g_traverse.f90
# The example will not be built when Intel compiler is used (EIP 2011/10/14)

if test "X$HAVE_FORTRAN_2003" = "Xyes"; then
  HAVE_FORTRAN_2003_NOTINTEL="yes"
  AC_MSG_CHECKING([if h5fc is an Intel Fortran compiler])
  if (${FC} -showconfig 2>&1 | grep 'Intel(R) Fortran ') > /dev/null; then
    HAVE_FORTRAN_2003_NOTINTEL="no"
  fi
  if test "X$HAVE_FORTRAN_2003_NOTINTEL" = "Xyes"; then
    echo "no"
  else
    echo "yes"
  fi 
fi
# End check if we have Fortran 2003 and Intel compiler.

AM_CONDITIONAL([FORTRAN_2003_CONDITIONAL_F], [test "X$HAVE_FORTRAN_2003" = "Xyes"])
AM_CONDITIONAL([FORTRAN_2003_NOTINTEL_CONDITIONAL_F], [test "X$HAVE_FORTRAN_2003_NOTINTEL" = "Xyes"])

# karl.w.schulz@intel.com (8/24/14)
#
# Define include path and library linkage from environment variables
# which should be provided via module loads. Note that we include
# linkage for both C, C++ and Fortran here.

CFLAGS="-I ${HDF5_INC} ${CFLAGS}"
CXXFLAGS="-I ${HDF5_INC} ${CXXFLAGS}"
FCFLAGS="-I ${HDF5_INC} ${FCLAGS}"
LDFLAGS="-L${HDF5_LIB} -lhdf5 -lhdf5_fortran -lhdf5_cpp ${LDFLAGS}"

# Set subdirectories
AC_CONFIG_FILES([Makefile ohpc_module/Makefile 1_6/makefile 1_6/C/makefile 1_6/C/H5G/makefile \
1_6/C/H5D/makefile 1_6/C/H5T/makefile 1_8/makefile 1_8/C/makefile 1_8/CXX/Makefile \
1_8/C/H5G/makefile 1_8/C/H5D/makefile 1_8/C/H5T/makefile 1_8/FORTRAN/makefile \
1_8/FORTRAN/H5G/makefile 1_8/FORTRAN/H5D/makefile 1_8/FORTRAN/H5T/makefile 1_10/makefile \
1_10/C/makefile 1_10/C/H5G/makefile 1_10/C/H5D/makefile 1_10/C/H5T/makefile \
1_10/FORTRAN/makefile 1_10/FORTRAN/H5G/makefile 1_10/FORTRAN/H5D/makefile 1_10/FORTRAN/H5T/makefile ])

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain........ : $LMOD_FAMILY_COMPILER
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
