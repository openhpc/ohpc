#
#
# AC_ALACRITY
#
#
#
dnl @synopsis AC_ALACRITY
dnl
dnl This macro test if ALACRITY is to be used.
dnl Use in C code:
dnl     #ifdef ALACRITY
dnl     #include "alacrity.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author David A. Boyuka II
dnl
AC_DEFUN([AC_ALACRITY],[

AC_MSG_NOTICE([=== checking for ALACRITY ===])

AM_CONDITIONAL(HAVE_ALACRITY,true)

AC_ARG_WITH([alacrity],
        [  --with-alacrity=DIR      Location of ALACRITY library],
        [ALACRITY_LDFLAGS="-L$withval/lib";
         ALACRITY_LIBS="-lalacrity";
         ALACRITY_CPPFLAGS="-I$withval/include";],
        [with_alacrity=no])

if test "x$with_alacrity" == "xno"; then

   AM_CONDITIONAL(HAVE_ALACRITY,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $ALACRITY_LIBS"
    LDFLAGS="$LDFLAGS $ALACRITY_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $ALACRITY_CPPFLAGS"

    dnl if test -z "${HAVE_ALACRITY_TRUE}"; then
    dnl        AC_CHECK_HEADERS(alacrity.h,
    dnl                ,
    dnl                [AM_CONDITIONAL(HAVE_ALACRITY,false)])
    dnl fi

    # Check for the ALACRITY library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [ALACRITY_LIBS="-lalacrity"],
    dnl        [AM_CONDITIONAL(HAVE_ALACRITY,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(ALACRITY_LIBS)
    AC_SUBST(ALACRITY_LDFLAGS)
    AC_SUBST(ALACRITY_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_ALACRITY_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_ALACRITY,1,[Define if you have ALACRITY.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_ALACRITY
