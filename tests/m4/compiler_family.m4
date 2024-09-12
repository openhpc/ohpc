# SYNOPSIS
#
#   Test for OHPC supported compiler toolchains
#
#   COMPILER_FAMILY()
#
# DESCRIPTION
#
#   Queries configuration environment to detect compiler toolchain
#   loaded via Lmod. Sets CC, CXX, and FC to match supported
#   toolchains.
#
# CONTRIBUTORS
#
#   Karl W. Schulz <karl@utexas.edu>

AC_DEFUN([OHPC_COMPILER_FAMILY],
[

AC_MSG_CHECKING([for loaded OHPC compiler toolchain])

if test "x$LMOD_FAMILY_COMPILER" = "xgnu"; then
   CC=gcc
   CXX=g++
   FC=gfortran
   AC_MSG_RESULT([gnu])
elif test "x$LMOD_FAMILY_COMPILER" = "xgnu12"; then
   CC=gcc
   CXX=g++
   FC=gfortran
   AC_MSG_RESULT([gnu12])
   OHPC_BLAS="-L${OPENBLAS_LIB} -lopenblas"
elif test "x$LMOD_FAMILY_COMPILER" = "xgnu14"; then
   CC=gcc
   CXX=g++
   FC=gfortran
   AC_MSG_RESULT([gnu14])
   OHPC_BLAS="-L${OPENBLAS_LIB} -lopenblas"
elif test "x$LMOD_FAMILY_COMPILER" = "xgnu13"; then
   CC=gcc
   CXX=g++
   FC=gfortran
   AC_MSG_RESULT([gnu13])
   OHPC_BLAS="-L${OPENBLAS_LIB} -lopenblas"
elif test "x$LMOD_FAMILY_COMPILER" = "xgnu9"; then
   CC=gcc
   CXX=g++
   FC=gfortran
   AC_MSG_RESULT([gnu9])
   OHPC_BLAS="-L${OPENBLAS_LIB} -lopenblas"
elif test "x$LMOD_FAMILY_COMPILER" = "xllvm10"; then
   CC=clang
   CXX=clang++
   # Use placeholder again; flang(f18) added to LLVM11 [JCS 9/15/20]
   FC=gfortran
   AC_MSG_RESULT([llvm10])
elif test "x$LMOD_FAMILY_COMPILER" = "xllvm9"; then
   CC=clang
   CXX=clang++
   # Use placeholder until F18 is added to LLVM [JCS 11/19/19]
   FC=gfortran
   AC_MSG_RESULT([llvm9])
elif test "x$LMOD_FAMILY_COMPILER" = "xintel"; then
   CC=icx
   CXX=icpx
   FC=ifx
   AC_MSG_RESULT([intel])
   OHPC_BLAS="-L${MKLROOT}/lib/intel64 -lmkl_rt"
elif test "x$LMOD_FAMILY_COMPILER" = "xacfl"; then
   CC=armclang
   CXX=armclang++
   FC=armflang
   AC_MSG_RESULT([arm])
   OHPC_BLAS="-larmpl_lp64"
else
   AC_MSG_RESULT([unknown])
   echo
   AC_MSG_ERROR([Unknown compiler family - please load a compiler toolchain.])
fi

AC_SUBST(OHPC_BLAS)

])
