# SYNOPSIS
#
#   Test if a desired environment variable is set for a particular module.
#
#   CHECK_ENV([VARIABLE,[module])
#
# DESCRIPTION
#
#   Queries configuration environment to detect compiler toolchain
#   loaded via Lmod. Sets CC, CXX, and FC to match supported
#   toolchains.
#
# CONTRIBUTORS
#
#   Karl W. Schulz <karl.w.schulz@intel.com>

AC_DEFUN([CHECK_ENV],
[

AC_MSG_CHECKING([for $1 environment variable])
if test "x$$1" = "x"; then
   AC_MSG_RESULT([no])
   echo 
   AC_MSG_ERROR([$1 not defined - please load $2 environment.])
else
   AC_MSG_RESULT([yes])
fi


])
