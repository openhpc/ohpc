#
#
# AC_RIDCOMPRESS
#
#
#
dnl @synopsis AC_RIDCOMPRESS
dnl
dnl This macro test if RIDCOMPRESS is to be used.
dnl Use in C code:
dnl     #ifdef RIDCOMPRESS
dnl     #include "pfordelta-c-interface.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author David A. Boyuka II
dnl
AC_DEFUN([AC_RIDCOMPRESS],[

AC_MSG_NOTICE([=== checking for RIDCOMPRESS ===])

AM_CONDITIONAL(HAVE_RIDCOMPRESS,true)

AC_ARG_WITH([ridcompress],
        [  --with-ridcompress=DIR      Location of RIDCOMPRESS library],
        [RIDCOMPRESS_LDFLAGS="-L$withval/ -L$withval/lib";
         RIDCOMPRESS_LIBS="-lridcompress -lstdc++";
         RIDCOMPRESS_CPPFLAGS="-I$withval/ -I$withval/include";],
        [with_RIDCOMPRESS=no])

if test "x$with_RIDCOMPRESS" == "xno"; then

   AM_CONDITIONAL(HAVE_RIDCOMPRESS,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $RIDCOMPRESS_LIBS"
    LDFLAGS="$LDFLAGS $RIDCOMPRESS_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $RIDCOMPRESS_CPPFLAGS"

    dnl if test -z "${HAVE_RIDCOMPRESS_TRUE}"; then
    dnl        AC_CHECK_HEADERS(pfordelta-c-interface.h,
    dnl                ,
    dnl                [AM_CONDITIONAL(HAVE_RIDCOMPRESS,false)])
    dnl fi

    # Check for the RIDCOMPRESS library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [RIDCOMPRESS_LIBS="-lRIDCOMPRESS"],
    dnl        [AM_CONDITIONAL(HAVE_RIDCOMPRESS,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(RIDCOMPRESS_LIBS)
    AC_SUBST(RIDCOMPRESS_LDFLAGS)
    AC_SUBST(RIDCOMPRESS_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_RIDCOMPRESS_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_RIDCOMPRESS,1,[Define if you have RIDCOMPRESS.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_RIDCOMPRESS
