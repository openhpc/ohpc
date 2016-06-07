dnl ######################################################################
dnl
dnl Finds IBM_PAMI 
dnl
dnl ######################################################################

AC_DEFUN([AC_PAMI],[
ac_pami_lib_ok=no

PAMI_CFLAGS=""
PAMI_CPPFLAGS=""
PAMI_LDFLAGS=""
PAMI_LIBS=""

AC_MSG_NOTICE([=== checking for IBM PAMI ===])

AM_CONDITIONAL(HAVE_PAMI,true)

AC_ARG_WITH(pami,
            [  --with-pami=DIR      Location of IBM PAMI],
    	    [ PAMI_CPPFLAGS="-I/bgsys/drivers/ppcfloor/comm/sys/include";
              PAMI_LDFLAGS="";
	      PAMI_LIBS="-lpthread";])

save_CPPFLAGS="$CPPFLAGS"
save_LDFLAGS="$LDFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$CPPFLAGS $PAMI_CPPFLAGS"
LDFLAGS="$LDFLAGS $PAMI_LDFLAGS"
LIBS="$LIBS $PAMI_LIBS"

dnl Check for the header file.
if test -z "${HAVE_PAMI_TRUE}"; then
	  AC_CHECK_HEADERS(pami.h,
		  ,
		  [AM_CONDITIONAL(HAVE_PAMI,false)])
fi

dnl Check for the library.
if test -z "${HAVE_PAMI_TRUE}"; then
	AC_MSG_CHECKING([if pami code can be linked])
	AC_TRY_LINK([#include "pami.h"],
		[pami_context_t context;
		 PAMI_Context_lock(context);
		 PAMI_Context_unlock(context);
		],
		[PAMI_LIBS="-lpthread"
		 AC_MSG_RESULT(yes)],
		[AM_CONDITIONAL(HAVE_PAMI, false)
		 AC_MSG_RESULT(no)])
fi

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(PAMI_CFLAGS)
AC_SUBST(PAMI_CPPFLAGS)
AC_SUBST(PAMI_LDFLAGS)
AC_SUBST(PAMI_LIBS)

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_PAMI_TRUE}"; then
	ac_pami_lib_ok=yes
 	ifelse([$1],,[AC_DEFINE(HAVE_PAMI,1,[Define if you have the PAMI.])],[$1])
	:
else
	$2
	:
fi
])dnl AC_PAMI
