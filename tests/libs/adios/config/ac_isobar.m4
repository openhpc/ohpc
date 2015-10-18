#
#
# AC_ISOBAR
#
#
#
dnl @synopsis AC_ISOBAR
dnl
dnl This macro test if ISOBAR is to be used.
dnl Use in C code:
dnl     #ifdef ISOBAR
dnl     #include "isobar.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Zhenhuan (Steve) Gong
dnl
AC_DEFUN([AC_ISOBAR],[

AC_MSG_NOTICE([=== checking for ISOBAR ===])

AM_CONDITIONAL(HAVE_ISOBAR,true)

AC_ARG_WITH(isobar,
        [  --with-isobar=DIR      Location of ISOBAR library],
        [ISOBAR_LDFLAGS="-L$withval/lib";
         ISOBAR_LIBS="-lisobar -lz";
         ISOBAR_CPPFLAGS="-I$withval/include";],
        [with_isobar=no])

if test "x$with_isobar" == "xno"; then

   AM_CONDITIONAL(HAVE_ISOBAR,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -lisobar -lz"
    LDFLAGS="$LDFLAGS $ISOBAR_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $ISOBAR_CPPFLAGS"

    if test -z "${HAVE_ISOBAR_TRUE}"; then
           AC_CHECK_HEADERS(isobar.h,
                   ,
                   [AM_CONDITIONAL(HAVE_ISOBAR,false)])
    fi

    # Check for the ISOBAR library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [ISOBAR_LIBS="-lisobar -lz"],
    dnl        [AM_CONDITIONAL(HAVE_ISOBAR,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(ISOBAR_LIBS)
    AC_SUBST(ISOBAR_LDFLAGS)
    AC_SUBST(ISOBAR_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_ISOBAR_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_ISOBAR,1,[Define if you have ISOBAR.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_ISOBAR
