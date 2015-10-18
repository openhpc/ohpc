#
#
# AC_LUSTRE
#
#
#
dnl @synopsis AC_LUSTRE
dnl
dnl This macro test if lustreapi.a can be used
dnl @version 2.0
dnl @author Qing Liu, UT
dnl @author Norbert Podhorszki, ORNL
dnl
AC_DEFUN([AC_LUSTRE],[

AC_MSG_NOTICE([=== checking for Lustre ===])

AM_CONDITIONAL(HAVE_LUSTRE,true)

dnl This is optional, if not given, do nothing
AC_ARG_WITH(lustre,
        [  --with-lustre[=DIR]      Location of lustre library],
        [:],[with_lustre=no])

if test "x${with_lustre}" == "xno"; then
       AM_CONDITIONAL(HAVE_LUSTRE,false)
fi

if test -z "${HAVE_LUSTRE_TRUE}"; then

    dnl AC_MSG_NOTICE([   debug: with_lustre="$with_lustre"])
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    
    if test "x$with_lustre" == "xyes"; then
        dnl No path given
        LUSTRE_CPPFLAGS=""
        LUSTRE_LIBS="-llustreapi"
        LUSTRE_LDFLAGS=""
    else
        dnl Path given, first try path/lib64
        LUSTRE_CPPFLAGS="-I${with_lustre}/include"
        LUSTRE_LIBS="-llustreapi"
        LUSTRE_LDFLAGS="-L${with_lustre}/lib64"
    fi

    LIBS="$LIBS $LUSTRE_LIBS"
    LDFLAGS="$LDFLAGS $LUSTRE_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $LUSTRE_CPPFLAGS"
    
    oldheader=no
    AC_CHECK_HEADERS([lustre/lustreapi.h],
                    ,
                    [AM_CONDITIONAL(HAVE_LUSTRE,false)])

    dnl if lustreapi.h is missing, we may still find 1.x lustre's liblustreapi.h
    if test -z "${HAVE_LUSTRE_FALSE}"; then
        AC_CHECK_HEADERS([lustre/liblustreapi.h],
                        [AM_CONDITIONAL(HAVE_LUSTRE,true)
                         oldheader=yes],
                        [AM_CONDITIONAL(HAVE_LUSTRE,false)])
    fi
    
    if test -z "${HAVE_LUSTRE_TRUE}"; then
        dnl Check for the lustre library
        AC_MSG_CHECKING([if lustre code can be linked with $LUSTRE_LDFLAGS])
        if test "${oldheader}" == "no" ; then
            AC_TRY_LINK(
                [#include <stdlib.h>
                 #include "lustre/lustreapi.h"
                 int fd, num_ost;
                 struct obd_uuid uuids[1024];],
                [llapi_lov_get_uuids(fd, uuids, &num_ost);],
                [AC_MSG_RESULT(yes)],
                [AM_CONDITIONAL(HAVE_LUSTRE,false)
                 AC_MSG_RESULT(no)
                ])
        else
            AC_TRY_LINK(
                [#include <stdlib.h>
                 #include "lustre/liblustreapi.h"
                 int fd, num_ost;
                 struct obd_uuid uuids[1024];],
                [llapi_lov_get_uuids(fd, uuids, &num_ost);],
                [AC_MSG_RESULT(yes)],
                [AM_CONDITIONAL(HAVE_LUSTRE,false)
                 AC_MSG_RESULT(no)
                ])
        fi
    fi

    dnl If Linking above failed, one reason might be that we looked in lib64/ instead of lib/
    if test -z "${HAVE_LUSTRE_FALSE}"; then
        if test "x$with_lustre" != "xyes"; then
            LUSTRE_LDFLAGS="-L${with_lustre}/lib"
            LDFLAGS="$save_LDFLAGS $LUSTRE_LDFLAGS"
            dnl Check for the lustre library
            AC_MSG_CHECKING([if lustre code can be linked with $LUSTRE_LDFLAGS])
            if test "${oldheader}" == "no" ; then
                AC_TRY_LINK(
                    [#include <stdlib.h>
                     #include "lustre/lustreapi.h"
                     int fd, num_ost;
                     struct obd_uuid uuids[1024];],
                    [llapi_lov_get_uuids(fd, uuids, &num_ost);],
                    [AM_CONDITIONAL(HAVE_LUSTRE,true)
                     AC_MSG_RESULT(yes)],
                    [AM_CONDITIONAL(HAVE_LUSTRE,false)
                    AC_MSG_RESULT(no)
                    ])
            else
                AC_TRY_LINK(
                    [#include <stdlib.h>
                     #include "lustre/liblustreapi.h"
                     int fd, num_ost;
                     struct obd_uuid uuids[1024];],
                    [llapi_lov_get_uuids(fd, uuids, &num_ost);],
                    [AM_CONDITIONAL(HAVE_LUSTRE,true)
                     AC_MSG_RESULT(yes)],
                    [AM_CONDITIONAL(HAVE_LUSTRE,false)
                    AC_MSG_RESULT(no)
                    ])
            fi
        fi
    fi
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    
    AC_SUBST(LUSTRE_LIBS)
    AC_SUBST(LUSTRE_LDFLAGS)
    AC_SUBST(LUSTRE_CPPFLAGS)
    
    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_LUSTRE_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_LUSTRE,1,[Define if you have LUSTRE.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_LUSTRE
