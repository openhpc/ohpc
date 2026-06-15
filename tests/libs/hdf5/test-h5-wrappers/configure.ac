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

AC_PREREQ([2.69])
AC_INIT([HDF5-tests-h5-wrappers],[0.10],[https://github.com/openhpc/ohpc])
dnl AM_INIT_AUTOMAKE takes a list of options that should be applied to
dnl every Makefile.am when automake is run.
AM_INIT_AUTOMAKE([foreign])
AC_CONFIG_HEADERS([config.h])

# Turn off automake rebuild rules so make doesn't try to run automake
# (which probably won't work).
AM_MAINTAINER_MODE

# Argument for static linking
AC_ARG_ENABLE(shared,
    [AS_HELP_STRING([--disable-shared],
        [force static linking])],
    [if test "$enableval" = "no"; then
        shared_suffix=""
    else
        shared_suffix=" -shlib"
    fi],
    [shared_suffix=" -shlib"])

# karl.w.schulz@intel.com (8/28/14)
# verify we have an hdf5 module loaded - indicated by existence of HDF5_DIR

AC_MSG_CHECKING([for HDF5_DIR environment variable])
if test "x$HDF5_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_MSG_ERROR([HDF5_DIR not defined - please load hdf5 environment.])
else
   AC_MSG_RESULT([yes])
fi

# Checks and sets HDF5 compiler wrappers.
if test "x$CC" = "x"; then
  AC_CHECK_PROG([H5CC_EXISTS], [h5cc], [yes], [no])
  if test "x$H5CC_EXISTS" = "xyes"; then
    H5C="h5cc"
  else
    AC_CHECK_PROG([H5PCC_EXISTS], [h5pcc], [yes], [no])
    if test "x$H5PCC_EXISTS" = "xyes"; then
      H5C="h5pcc"
    else
      AC_MSG_ERROR([failed to find h5cc or h5pcc])
    fi
  fi
else
  H5C="$CC"
fi
AC_PROG_CC([$H5C])

if test "x$FC" = "x"; then
  AC_CHECK_PROG([H5FC_EXISTS], [h5fc], [yes], [no])
  if test "x$H5FC_EXISTS" = "xyes"; then
    H5FC="h5fc"
  else
    AC_CHECK_PROG([H5PFC_EXISTS], [h5pfc], [yes], [no])
    if test "x$H5PFC_EXISTS" = "xyes"; then
      H5FC="h5pfc"
    else
      AC_MSG_ERROR([failed to find h5fc or h5pfc])
    fi
  fi
else
  H5FC="$FC"
fi
AC_PROG_FC([$H5FC])

AC_PROG_CXX([h5c++])

# Add the suffix to CC for shared linking.  Can't just set as an option
# because it must be first.
if test "$shared_suffix" && test ! `echo ${CC} | grep "$shared_suffix"`; then
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

# Determine the version of the HDF5 Library
AC_MSG_CHECKING([the version of the HDF5 Library])

AC_SUBST([H5_LIBVER])
H5_LIBVER=$(${H5C} -showconfig | grep -i "HDF5 Version:" | sed 's/^.* //g' | sed 's/[-].*//g')
AC_MSG_RESULT([$H5_LIBVER])

# Test whether one version ($1) is greater than other ($2).
function version_gt {
    test "`printf '%s\n' "$@" | sort -V | head -n 1`" != "$1";
}

# Check if a Fortran 2003 compiler was used for h5fc
HAVE_FORTRAN_2003="no"

AC_MSG_CHECKING([if h5fc was compiled with Fortran 2003 enabled])

# if HDF5 version is 1.8 and HDF5 used a 2003 Fortran compiler.
if (${FC} -showconfig 2>&1 | grep 'Fortran 2003 Compiler: yes') > /dev/null; then
  HAVE_FORTRAN_2003="yes"
else
# check if HDF5 version is > 1.8
 if version_gt "$H5_LIBVER" "1.9.0"; then
     HAVE_FORTRAN_2003="yes"
 fi
fi

AC_MSG_RESULT([$HAVE_FORTRAN_2003])

FORTRAN_2003_CONDITIONAL_F="X$HAVE_FORTRAN_2003"
AC_SUBST([FORTRAN_2003_CONDITIONAL_F])
AM_CONDITIONAL([FORTRAN_2003_CONDITIONAL_F], [test "X$HAVE_FORTRAN_2003" = "Xyes"])

# Check if we have Fortran 2003 and Intel compiler; Intel doesn't work with RECURSIVE used by h5ex_g_traverse.f90
# The example will not be built when Intel compiler is used (EIP 2011/10/14)
# Also disable h5ex_g_traverse_F03 for HDF5 >= 2.0 where H5O_info_t%addr was removed.

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

# HDF5 >= 2.0 removed H5O_info_t%addr; h5ex_g_traverse_F03 cannot compile
if version_gt "$H5_LIBVER" "1.99.0"; then
  echo "HDF5 >= 2.0 detected, disabling h5ex_g_traverse_F03"
  HAVE_FORTRAN_2003_NOTINTEL="no"
fi

AM_CONDITIONAL([FORTRAN_2003_NOTINTEL_CONDITIONAL_F], [test "X$HAVE_FORTRAN_2003_NOTINTEL" = "Xyes"])

# Set subdirectories
AC_CONFIG_FILES([makefile 1_6/makefile 1_6/C/makefile 1_6/C/H5G/makefile \
1_6/C/H5D/makefile 1_6/C/H5T/makefile 1_8/makefile 1_8/C/makefile \
1_8/C/H5G/makefile 1_8/C/H5D/makefile 1_8/C/H5T/makefile 1_8/FORTRAN/makefile 1_8/CXX/Makefile \
1_8/FORTRAN/H5G/makefile 1_8/FORTRAN/H5D/makefile 1_8/FORTRAN/H5T/makefile 1_10/makefile \
1_10/C/makefile 1_10/C/H5G/makefile 1_10/C/H5D/makefile 1_10/C/H5T/makefile \
1_10/FORTRAN/makefile 1_10/FORTRAN/H5G/makefile 1_10/FORTRAN/H5D/makefile 1_10/FORTRAN/H5T/makefile ])

AC_CONFIG_FILES([1_10/C/H5D/test.sh], [chmod +x 1_10/C/H5D/test.sh])
AC_CONFIG_FILES([1_10/C/H5G/test.sh], [chmod +x 1_10/C/H5G/test.sh])
AC_CONFIG_FILES([1_10/C/H5T/test.sh], [chmod +x 1_10/C/H5T/test.sh])

CFLAGS="${CFLAGS} -Wno-unused-but-set-variable -Wno-format -Wno-switch"
FCFLAGS="${FCFLAGS} -Wno-unused-variable -Wno-conversion"

# Configure
AC_OUTPUT

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version............... : $PACKAGE-$VERSION
echo OHPC compiler toolchain....... : $LMOD_FAMILY_COMPILER
echo
echo C compiler.................... : $(which $H5C)
echo Fortran compiler ............. : $(which $H5FC)
echo
echo C compiler flags.............. : $CFLAGS
echo Fortran compiler flags........ : $FCFLAGS
echo
echo '-------------------------------------------------------------------------------------------------------------'
