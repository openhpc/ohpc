#
#
# AC_APLOD
#
#
#
dnl @synopsis AC_APLOD
dnl
dnl This macro test if APLOD is to be used.
dnl Use in C code:
dnl     #ifdef APLOD
dnl     #include "aplod.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Zhenhuan (Steve) Gong
dnl
AC_DEFUN([AC_APLOD],[

AC_MSG_NOTICE([=== checking for APLOD ===])

AM_CONDITIONAL(HAVE_APLOD,true)

AC_ARG_WITH(aplod,
        [  --with-aplod=DIR      Location of APLOD library],
        [APLOD_LDFLAGS="-L$withval/lib";
         APLOD_LIBS="-laplod -lz";
         APLOD_CPPFLAGS="-I$withval/include";],
        [with_aplod=no])

if test "x$with_aplod" == "xno"; then

   AM_CONDITIONAL(HAVE_APLOD,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -laplod -lz"
    LDFLAGS="$LDFLAGS $APLOD_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $APLOD_CPPFLAGS"

    if test -z "${HAVE_APLOD_TRUE}"; then
           AC_CHECK_HEADERS(aplod.h,
                   ,
                   [AM_CONDITIONAL(HAVE_APLOD,false)])
    fi

    # Check for the APLOD library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [APLOD_LIBS="-laplod -lz"],
    dnl        [AM_CONDITIONAL(HAVE_APLOD,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(APLOD_LIBS)
    AC_SUBST(APLOD_LDFLAGS)
    AC_SUBST(APLOD_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_APLOD_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_APLOD,1,[Define if you have APLOD.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_APLOD
