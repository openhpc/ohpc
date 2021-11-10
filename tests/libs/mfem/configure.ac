#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.
#

AC_PREREQ(2.59)
AC_INIT([mfem], [3.3.2], [https://github.com/openhpc/ohpc])
AC_CONFIG_MACRO_DIR([m4])
AM_INIT_AUTOMAKE([foreign])
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

# verify we have necessary environment variables set
AC_MSG_CHECKING([for MFEM_DIR environment variable])
if test "x$MFEM_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([MFEM_DIR not defined - please load mfem environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for HYPRE_DIR environment variable])
if test "x$HYPRE_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([HYPRE_DIR not defined - please load hypre environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for METIS_DIR environment variable])
if test "x$METIS_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([METIS_DIR not defined - please load metis environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for NETCDF_DIR environment variable])
if test "x$NETCDF_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([NETCDF_DIR not defined - please load netcdf environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for PETSC_DIR environment variable])
if test "x$PETSC_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([PETSC_DIR not defined - please load petsc environment.])
else
   AC_MSG_RESULT([yes])
fi

# verify we have necessary environment variables set
AC_MSG_CHECKING([for SUPERLD_DIST_DIR environment variable])
if test "x$SUPERLU_DIST_DIR" = "x"; then
   AC_MSG_RESULT([no])
   echo
   AC_ERROR([SUPERLU_DIST_DIR not defined - please load superlu_dist environment.])
else
   AC_MSG_RESULT([yes])
fi

OHPC_COMPILER_FAMILY

# test compilers
CXX=mpicxx

AC_PROG_CXX

CXXFLAGS="-I${HYPRE_INC} -I${MFEM_INC} -I${PETSC_INC} -I${SUPERLU_DIST_INC} ${CXXFLAGS} -std=c++11"
LDFLAGS="-L${MFEM_LIB}"
#LDFLAGS="-L${MFEM_LIB} -L${HYPRE_LIB} -L${METIS_LIB} -L${NETCDF_LIB} -L${PETSC_LIB} -L${SUPERLU_DIST_LIB}"
#LIBS="-lpetsc -lHYPRE -lsuperlu_dist -lnetcdf -lmfem"

# Set subdirectories
AC_CONFIG_FILES(Makefile tests/Makefile tests/serial/Makefile tests/parallel/Makefile )

# Configure
AC_OUTPUT()

echo
echo '-------------------------------------------------- SUMMARY --------------------------------------------------'
echo
echo Package version................. : $PACKAGE-$VERSION
echo OHPC compiler toolchain......... : $LMOD_FAMILY_COMPILER
echo OHPC MPI toolchain.............. : $LMOD_FAMILY_MPI
echo
echo CXX compiler.................... : `which $CXX`
echo CXX compiler flags.............. : $CXXFLAGS
echo 
echo '-------------------------------------------------------------------------------------------------------------'
