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

AC_PREREQ(2.69)
AC_INIT(HDF5-tests-env-variables, 0.20, [https://github.com/openhpc/ohpc])
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

# Turn off automake rebuild rules so make doesn't try to run automake
# (which probably won't work).
AM_MAINTAINER_MODE

# Checks for typedefs, structures, and compiler characteristics.
AC_C_CONST

# Determine the version of the HDF5 Library
AC_MSG_CHECKING([the version of the HDF5 Library])

AC_SUBST([H5_LIBVER])
H5_LIBVER=1.12.2
AC_MSG_RESULT([$H5_LIBVER])

AC_MSG_CHECKING([the API mapping of the HDF5 Library])
AC_SUBST([H5_APIVER])
H5_APIVER=1_12
AC_MSG_RESULT([$H5_APIVER])

#if version is unreleased then bump to next official release.

H5_MAJORVER=$(echo $H5_LIBVER | cut -f1 -d'.'  | sed -E 's/\./_/g')
H5_MINORVER=$(echo $H5_LIBVER | cut -f2 -d'.'  | sed -E 's/\./_/g')
H5_RELEASEVER=$(echo $H5_LIBVER | cut -f3 -d'.'  | sed -E 's/\./_/g')

#used in fortran version checks
H5_VERS_MAJOR=$H5_MAJORVER
H5_VERS_MINOR=$H5_MINORVER
H5_VERS_RELEASE=$H5_RELEASEVER

AC_SUBST([H5_VERS_MAJOR])
AC_SUBST([H5_VERS_MINOR])
AC_SUBST([H5_VERS_RELEASE])

AC_SUBST([H5_LIBVER_DIR])
rem=$(( $H5_MINORVER % 2 ))
if test "$rem" != "0";then
 H5_MINORVER=$(($H5_MINORVER+1))
fi
H5_LIBVER_DIR=${H5_MAJORVER}${H5_MINORVER}

# Test whether one version ($1) is greater than or equal to other ($2).
function version_gt {
    test "`printf '%s\n' "$@" | sort -V | head -n 1`" != "$1";
}

if test ! "$H5EX_G" && test ! "$H5EX_D" && test ! "$H5EX_T";then
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

# Check if we have Fortran 2003 and Intel compiler; Intel doesn't work with RECURSIVE used by h5ex_g_traverse.F90
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
AC_CONFIG_FILES([Makefile
                 ohpc_module/Makefile
                 C/Makefile
                 C/H5D/Makefile
                 C/H5G/Makefile
                 C/H5T/Makefile
                 FORTRAN/Makefile
                 FORTRAN/H5D/h5_version.h
                 FORTRAN/H5D/Makefile
                 FORTRAN/H5G/Makefile
                 FORTRAN/H5T/Makefile
        ])

AC_CONFIG_FILES([C/H5D/maketestref.sh], [chmod +x C/H5D/maketestref.sh])
AC_CONFIG_FILES([C/H5G/maketestref.sh], [chmod +x C/H5G/maketestref.sh])
AC_CONFIG_FILES([C/H5T/maketestref.sh], [chmod +x C/H5T/maketestref.sh])
AC_CONFIG_FILES([FORTRAN/H5D/maketestref.sh], [chmod +x FORTRAN/H5D/maketestref.sh])
AC_CONFIG_FILES([FORTRAN/H5G/maketestref.sh], [chmod +x FORTRAN/H5G/maketestref.sh])
AC_CONFIG_FILES([FORTRAN/H5T/maketestref.sh], [chmod +x FORTRAN/H5T/maketestref.sh])

AC_CONFIG_FILES([C/H5D/test.sh], [chmod +x C/H5D/test.sh])
AC_CONFIG_FILES([C/H5G/test.sh], [chmod +x C/H5G/test.sh])
AC_CONFIG_FILES([C/H5T/test.sh], [chmod +x C/H5T/test.sh])
AC_CONFIG_FILES([FORTRAN/H5D/test.sh], [chmod +x FORTRAN/H5D/test.sh])
AC_CONFIG_FILES([FORTRAN/H5G/test.sh], [chmod +x FORTRAN/H5G/test.sh])
AC_CONFIG_FILES([FORTRAN/H5T/test.sh], [chmod +x FORTRAN/H5T/test.sh])

# Configure
AC_OUTPUT

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
echo
echo '-------------------------------------------------------------------------------------------------------------'
