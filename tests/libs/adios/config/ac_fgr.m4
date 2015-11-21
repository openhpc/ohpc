#
#
# AC_FGR
#
#
#
dnl @synopsis AC_FGR
dnl
dnl This macro test if fgr (for Titan only) is to be used. 
dnl Use in C code:
dnl     #ifdef FGR
dnl     #include "fgr.h"
dnl     #endif
dnl
dnl @version 1.0
dnl @author Qing Liu, ORNL
dnl
AC_DEFUN([AC_FGR],[

AC_MSG_NOTICE([=== checking for FGR ===])

AM_CONDITIONAL(HAVE_FGR,true)

AC_ARG_WITH(fgr,
        [  --with-fgr=DIR      Location of FGR library],
        [FGR_LDFLAGS="-L$withval/lib";
         FGR_LIBS="-lfgr";
         FGR_CPPFLAGS="-I$withval/include";],
        [with_fgr=no])

if test "x$with_fgr" == "xno"; then

   AM_CONDITIONAL(HAVE_FGR,false)

else

    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS -lfgr"
    LDFLAGS="$LDFLAGS $FDR_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $FDR_CPPFLAGS"
    
    # Check for the FGR library and headers
    dnl AC_TRY_COMPILE([struct obd_uuid {char uuid[40];};int fd, num_ost;struct obd_uuid uuids[1024];],
    dnl        [llapi_lov_get_uuids(fd, uuids, &num_ost);],
    dnl        [LUSTRE_LIBS="-llustreapi"],
    dnl        [AM_CONDITIONAL(HAVE_LUSTRE,false)])
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    
    AC_SUBST(FGR_LIBS)
    AC_SUBST(FGR_LDFLAGS)
    AC_SUBST(FGR_CPPFLAGS)
    
    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_FGR_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_FGR,1,[Define if you have FGR.])],[$1])
            :
    else
            $2
            :
    fi
fi
])dnl AC_FGR
