AC_DEFUN([AX_CHECK_USER],[
AC_MSG_CHECKING([for user $1])

ROOT_ENABLED=0

if test x$USER = xroot ; then
    AC_MSG_RESULT([yes])
    ROOT_ENABLED=1
else
    AC_MSG_RESULT([no])
fi

AM_CONDITIONAL(ROOT_ENABLED,test x$ROOT_ENABLED = x1) 


])

