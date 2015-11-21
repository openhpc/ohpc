#
#
# AC_SZIP
#
#
#
dnl @synopsis AC_SZIP
dnl
dnl This macro test if SZIP is to be used.
dnl Use in C code:
dnl     #ifdef SZIP
dnl     #include "szlib.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Zhenhuan (Steve) Gong
dnl
AC_DEFUN([AC_SZIP],[

AC_MSG_NOTICE([=== checking for SZIP ===])

AM_CONDITIONAL(HAVE_SZIP,true)

AC_ARG_WITH(szip,
        [  --with-szip=DIR      Location of SZIP library],
        [SZIP_LDFLAGS="-L$withval/lib";
         SZIP_LIBS="-lsz";
         SZIP_CPPFLAGS="-I$withval/include";],
        [with_szip=no])

if test "x$with_szip" == "xno"; then

   AM_CONDITIONAL(HAVE_SZIP,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -lsz"
    LDFLAGS="$LDFLAGS $SZIP_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $SZIP_CPPFLAGS"

    if test -z "${HAVE_SZIP_TRUE}"; then
           AC_CHECK_HEADERS(szlib.h,
                   ,
                   [AM_CONDITIONAL(HAVE_SZIP,false)])
    fi

    # Check for the SZIP library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [SZIP_LIBS="-lsz"],
    dnl        [AM_CONDITIONAL(HAVE_SZIP,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(SZIP_LIBS)
    AC_SUBST(SZIP_LDFLAGS)
    AC_SUBST(SZIP_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_SZIP_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_SZIP,1,[Define if you have SZIP.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_SZIP
