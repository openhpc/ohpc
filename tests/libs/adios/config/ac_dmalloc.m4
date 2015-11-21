#
#
# AC_DMALLOC
#
#
#
dnl @synopsis AC_DMALLOC
dnl
dnl This macro test if dmalloc is to be used. 
dnl Use in C code:
dnl     #ifdef DMALLOC
dnl     #include "dmalloc.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Norbert Podhorszki, ORNL
dnl
AC_DEFUN([AC_DMALLOC],[

AM_CONDITIONAL(HAVE_DMALLOC,true)

AC_ARG_WITH(dmalloc,
        [  --with-dmalloc=DIR      Location of dmalloc library],
        [DMALLOC_LDFLAGS="-L$withval/lib";
         DMALLOC_CPPFLAGS="-I$withval/include";],
        [with_dmalloc=no])

dnl If --without-hdf5 was given set HAVE_HDF5 to false and do nothing more
dnl If nothing was given, then too, do nothing 
if test "x$with_dmalloc" == "xno"; then

    AM_CONDITIONAL(HAVE_DMALLOC,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -ldmalloc"
    LDFLAGS="$LDFLAGS $DMALLOC_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $DMALLOC_CPPFLAGS"

    if test -z "${HAVE_DMALLOC_TRUE}"; then
        AC_CHECK_HEADERS(dmalloc.h,
                ,
                [AM_CONDITIONAL(HAVE_DMALLOC,false)])
    fi

    # Check for the dmalloc library and headers
    AC_TRY_COMPILE([#include "dmalloc.h"],
            [char * s; s=malloc(sizeof(char)*10); free(s);],
            [DMALLOC_LIBS="-ldmallocth"],
            [AM_CONDITIONAL(HAVE_DMALLOC,false)])

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"

    AC_SUBST(DMALLOC_LIBS)
    AC_SUBST(DMALLOC_LDFLAGS)
    AC_SUBST(DMALLOC_CPPFLAGS)

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_DMALLOC_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_DMALLOC,1,[Define if you have DMALLOC.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_DMALLOC
