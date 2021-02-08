# SYNOPSIS
#
#   Test for package version.
#
#   LMOD_PACKAGE_VER(<module-name>)
#
# DESCRIPTION
#
#   Queries the module version information for a specified package
#   module.  Assume module is already loaded.
#
# CONTRIBUTORS
#
#   Karl W. Schulz <karl@utexas.edu>

AC_DEFUN([LMOD_PACKAGE_VER],
[

   name=$1
   AC_MSG_CHECKING([for $name module version])
   MODVERSION=`module -t list $name | sed "s|$name/||"` 
   AC_MSG_RESULT([$MODVERSION])   
   AC_SUBST(MODVERSION)

])
