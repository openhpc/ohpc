#
#
# AC_GLIB
#
#
#
dnl @synopsis AC_GLIB
dnl
dnl This macro test if glib (for Titan only) is to be used. 
dnl Use in C code:
dnl     #ifdef GLIB
dnl     #include "glib.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Qing Liu, ORNL
dnl
AC_DEFUN([AC_GLIB],[

AC_MSG_NOTICE([=== checking for GLIB ===])

AM_CONDITIONAL(HAVE_GLIB,true)

AC_ARG_WITH(glib,
        [  --with-glib=DIR      Location of GLIB],
        [GLIB_LDFLAGS="-L$withval/lib";
         GLIB_LIBS="-lglib-2.0";
         GLIB_CPPFLAGS="-I$withval/include/glib-2.0 -I$withval/lib/glib-2.0/include";],
        [with_glib=no])

if test "x$with_glib" == "xno"; then

   AM_CONDITIONAL(HAVE_GLIB,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -lglib-2.0"
    LDFLAGS="$LDFLAGS $GLIB_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $GLIB_CPPFLAGS"
    
    # Check for the GLIB library and headers
    AC_TRY_COMPILE([#include "glib.h"],
                   [GHashTable * ght],
                   [GLIB_LIBS="-lglib-2.0"],
                   [AM_CONDITIONAL(HAVE_GLIB,false)])
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    
    AC_SUBST(GLIB_LIBS)
    AC_SUBST(GLIB_LDFLAGS)
    AC_SUBST(GLIB_CPPFLAGS)
    
    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_GLIB_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_GLIB,1,[Define if you have GLIB.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_GLIB
