#
#
# AC_ZLIB
#
#
#
dnl @synopsis AC_ZLIB
dnl
dnl This macro test if ZLIB is to be used.
dnl Use in C code:
dnl     #ifdef ZLIB
dnl     #include "zlib.h"
dnl     #endif
dnl
dnl @version 2.0
dnl @author Zhenhuan (Steve) Gong
dnl @author Norbert Podhorszki
dnl
AC_DEFUN([AC_ZLIB],[

AC_MSG_NOTICE([=== checking for ZLIB ===])

AM_CONDITIONAL(HAVE_ZLIB,true)

AC_ARG_WITH(zlib,
        [  --with-zlib=DIR      Location of ZLIB library],
        [:], [with_zlib=no])

if test "x$with_zlib" == "xno"; then

   AM_CONDITIONAL(HAVE_ZLIB,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"

    if test "x$with_zlib" == "xyes"; then
        dnl No path given
        ZLIB_CPPFLAGS=""
        ZLIB_LDFLAGS=""
        ZLIB_LIBS="-lz"
    else
        dnl Path given, first try path/lib64
        ZLIB_CPPFLAGS="-I$withval/include"
        ZLIB_LDFLAGS="-L$withval/lib64"
        ZLIB_LIBS="-lz"
    fi

    LIBS="$LIBS $ZLIB_LIBS"
    LDFLAGS="$LDFLAGS $ZLIB_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $ZLIB_CPPFLAGS"

    dnl Find header file first
    AC_CHECK_HEADERS(zlib.h,
              ,
              [AM_CONDITIONAL(HAVE_ZLIB,false)])

    if test -z "${HAVE_ZLIB_TRUE}"; then
        dnl Try to link an example now
        AC_MSG_CHECKING([if zlib code can be linked with $ZLIB_LDFLAGS])
        AC_TRY_LINK(
            [#include <stdlib.h>
             #include "zlib.h"],
            [Bytef* in, *out;
             uLongf in_len, *out_len;
             int level = 5;
             int zerr = compress2 (out, out_len, in, in_len, level);
             return (zerr != Z_OK);],
            [AC_MSG_RESULT(yes)],
            [AM_CONDITIONAL(HAVE_ZLIB,false)
             AC_MSG_RESULT(no)
            ])
            
        dnl If linking above failed, one reason might be that we looked in lib64/
        dnl instead of lib/
        if test -z "${HAVE_ZLIB_FALSE}"; then
            if test "x$with_lustre" != "xyes"; then
            ZLIB_LDFLAGS="-L$withval/lib"
            LDFLAGS="$LDFLAGS $ZLIB_LDFLAGS"
            AC_MSG_CHECKING([if zlib code can be linked with $ZLIB_LDFLAGS])
            AC_TRY_LINK(
                [#include <stdlib.h>
                 #include "zlib.h"],
                [Bytef* in, *out;
                 uLongf in_len, *out_len;
                 int level = 5;
                 int zerr = compress2 (out, out_len, in, in_len, level);
                 return (zerr != Z_OK);],
                [AC_MSG_RESULT(yes)],
                [AM_CONDITIONAL(HAVE_ZLIB,false)
                 AC_MSG_RESULT(no)
                ])
            fi
        fi
    fi

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(ZLIB_LIBS)
    AC_SUBST(ZLIB_LDFLAGS)
    AC_SUBST(ZLIB_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_ZLIB_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_ZLIB,1,[Define if you have ZLIB.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_ZLIB
