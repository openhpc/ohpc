dnl ######################################################################
dnl
dnl Finds IBM_DCMF 
dnl
dnl ######################################################################

AC_DEFUN([AC_DCMF],[
ac_dcmf_lib_ok=no

DCMF_CFLAGS=""
DCMF_CPPFLAGS=""
DCMF_LDFLAGS=""
DCMF_LIBS=""

AC_MSG_NOTICE([=== checking for IBM DCMF ===])

AM_CONDITIONAL(HAVE_DCMF,true)

AC_ARG_WITH(dcmf,
            [  --with-dcmf=DIR      Location of IBM DCMF],
    	    [ DCMF_CPPFLAGS="-I$withval/arch/include -I$withval/comm/include";
              DCMF_LDFLAGS="-L$withval/comm/lib -L$withval/runtime/SPI";
	      DCMF_LIBS="-ldcmf.cnk -ldcmfcoll.cnk -lpthread -lrt -lSPI.cna";])

save_CPPFLAGS="$CPPFLAGS"
save_LDFLAGS="$LDFLAGS"
save_LIBS="$LIBS"
CPPFLAGS="$CPPFLAGS $DCMF_CPPFLAGS"
LDFLAGS="$LDFLAGS $DCMF_LDFLAGS"
LIBS="$LIBS $DCMF_LIBS"

dnl Check for the header file.
if test -z "${HAVE_DCMF_TRUE}"; then
	  AC_CHECK_HEADERS(dcmf.h,
		  ,
		  [AM_CONDITIONAL(HAVE_DCMF,false)])
fi

dnl Check for the library.
if test -z "${HAVE_DCMF_TRUE}"; then
	AC_MSG_CHECKING([if dcmf code can be linked])
	AC_TRY_LINK([#include "dcmf.h"],
		[unsigned ret;
		 ret = DCMF_Messager_initialize();
		 ret = DCMF_Messager_finalize();],
		[DCMF_LIBS="-ldcmf.cnk -ldcmfcoll.cnk -lpthread -lrt -lSPI.cna"
		 AC_MSG_RESULT(yes)],
		[AM_CONDITIONAL(HAVE_DCMF, false)
		 AC_MSG_RESULT(no)])
fi

LIBS="$save_LIBS"
LDFLAGS="$save_LDFLAGS"
CPPFLAGS="$save_CPPFLAGS"

AC_SUBST(DCMF_CFLAGS)
AC_SUBST(DCMF_CPPFLAGS)
AC_SUBST(DCMF_LDFLAGS)
AC_SUBST(DCMF_LIBS)

dnl Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test -z "${HAVE_DCMF_TRUE}"; then
	ac_dcmf_lib_ok=yes
 	ifelse([$1],,[AC_DEFINE(HAVE_DCMF,1,[Define if you have the DCMF.])],[$1])
	:
else
	$2
	:
fi
])dnl AC_DCMF
