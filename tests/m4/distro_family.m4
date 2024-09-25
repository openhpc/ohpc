# SYNOPSIS
#
#   Test for OHPC supported compiler toolchains
#
#   DISTRO_FAMILY()
#
# DESCRIPTION
#
#   Queries configuration environment to detect Linux distro
#   family from a list of supported variants.
#
# CONTRIBUTORS
#
#   Karl W. Schulz <karl@oden.utexas.edu>

AC_DEFUN([OHPC_DISTRO_FAMILY],
[

AC_MSG_CHECKING([for Linux distro family])

if test -r "/etc/os-release"; then
   if grep -q "CentOS" /etc/os-release; then
      AC_MSG_RESULT([CentOS])
      DISTRO_FAMILY=CentOS
   elif grep -q "Red Hat" /etc/os-release; then
      AC_MSG_RESULT([RHEL])
      DISTRO_FAMILY=RHEL
   elif grep -q "platform:el9" /etc/os-release; then
      AC_MSG_RESULT([CentOS9])
      DISTRO_FAMILY=CentOS
   elif grep -q "SLES" /etc/os-release; then
      AC_MSG_RESULT([SLES])
      DISTRO_FAMILY=SLES
   elif grep -q "openSUSE Leap" /etc/os-release; then
      AC_MSG_RESULT([openSUSE Leap])
      DISTRO_FAMILY=SLES
   elif grep -q "openEuler" /etc/os-release; then
      AC_MSG_RESULT([openEuler])
      DISTRO_FAMILY=openEuler
   else  
      AC_MSG_RESULT([unknown])
      echo
      AC_MSG_ERROR(Unknown or unsupported distribution specified in /etc/os-release.)
   fi

   AC_SUBST(DISTRO_FAMILY)

else
   AC_MSG_RESULT([unknown])
   echo
   AC_MSG_ERROR(Expected /etc/os-release file not present.)
fi

])
