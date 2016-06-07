#
#
# AC_BZIP2
#
#
#
dnl @synopsis AC_BZIP2
dnl
dnl This macro test if BZIP2 is to be used.
dnl Use in C code:
dnl     #ifdef BZIP2
dnl     #include "bzlib.h"
dnl     #endif
dnl
dnl @version 2.0
dnl @author Zhenhuan (Steve) Gong
dnl dnl @author Norbert Podhorszki
dnl
AC_DEFUN([AC_BZIP2],[

AC_MSG_NOTICE([=== checking for BZIP2 ===])

AM_CONDITIONAL(HAVE_BZIP2,true)

AC_ARG_WITH(bzip2,
        [  --with-bzip2=DIR      Location of BZIP2 library],
        [:], [with_bzip2=no])

if test "x$with_bzip2" == "xno"; then

   AM_CONDITIONAL(HAVE_BZIP2,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"

    if test "x$with_bzip2" == "xyes"; then
        dnl No path given
        BZIP2_CPPFLAGS=""
        BZIP2_LDFLAGS=""
        BZIP2_LIBS="-lbz2"
    else
        dnl Path given, first try path/lib64
        BZIP2_CPPFLAGS="-I$withval/include"
        BZIP2_LDFLAGS="-L$withval/lib64"
        BZIP2_LIBS="-lbz2"
    fi

    LIBS="$LIBS $BZIP2_LIBS"
    LDFLAGS="$LDFLAGS $BZIP2_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $BZIP2_CPPFLAGS"

    if test -z "${HAVE_BZIP2_TRUE}"; then
           AC_CHECK_HEADERS(bzlib.h,
                   ,
                   [AM_CONDITIONAL(HAVE_BZIP2,false)])
    fi

    if test -z "${HAVE_BZIP2_TRUE}"; then
        dnl Try to link an example now
        AC_MSG_CHECKING([if bzip2 code can be linked with $BZIP2_LDFLAGS])
        AC_TRY_LINK(
            [#include <stdlib.h>
             #include "bzlib.h"],
            [char* in, *out;
             unsigned int in_len, *out_len;
             int blocksize100k = 5;
             int bzerr = BZ2_bzBuffToBuffCompress (
                           out, out_len, in, in_len, blocksize100k, 0, 30);
             return (bzerr != BZ_OK);],
            [AC_MSG_RESULT(yes)],
            [AM_CONDITIONAL(HAVE_BZIP2,false)
             AC_MSG_RESULT(no)
            ])

        dnl If linking above failed, one reason might be that we looked in lib64/
        dnl instead of lib/
        if test -z "${HAVE_BZIP2_FALSE}"; then
            if test "x$with_lustre" != "xyes"; then
            BZIP2_LDFLAGS="-L$withval/lib"
            LDFLAGS="$LDFLAGS $BZIP2_LDFLAGS"
            AC_MSG_CHECKING([if bzip2 code can be linked with $BZIP2_LDFLAGS])
            AC_TRY_LINK(
                [#include <stdlib.h>
                 #include "bzlib.h"],
                [char* in, *out;
                 unsigned int in_len, *out_len;
                 int blocksize100k = 5;
                 int bzerr = BZ2_bzBuffToBuffCompress (
                               out, out_len, in, in_len, blocksize100k, 0, 30);
                 return (bzerr != BZ_OK);],
                [AC_MSG_RESULT(yes)],
                [AM_CONDITIONAL(HAVE_BZIP2,false)
                 AC_MSG_RESULT(no)
                ])
            fi
        fi
    fi
 

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(BZIP2_LIBS)
    AC_SUBST(BZIP2_LDFLAGS)
    AC_SUBST(BZIP2_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_BZIP2_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_BZIP2,1,[Define if you have BZIP2.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_BZIP2
