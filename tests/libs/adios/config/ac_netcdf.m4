dnl ######################################################################
dnl
dnl Finds netCDF
dnl
dnl ######################################################################

AC_DEFUN([AC_NETCDF],
[
AC_MSG_NOTICE([=== checking for NetCDF ===])

AM_CONDITIONAL(HAVE_NETCDF,true)

AC_ARG_WITH(netcdf,
            [  --with-netcdf=<location of NetCDF installation>],
            [NETCDF_DIR=$withval], [with_netcdf=check])

dnl If --without-netcdf was given set HAVE_NETCDF to false and do nothing more
if test "x$with_netcdf" == "xno"; then

   AM_CONDITIONAL(HAVE_NETCDF,false)

else

    ac_use_cray_netcdf=no

    dnl allow args --with-netcdf incdir and --with-netcdf-libdir
    AC_ARG_WITH(netcdf-incdir,
                [  --with-netcdf-incdir=<location of NetCDF includes>],
                [NETCDF_INCDIR=$withval
                 with_netcdf=detailed])
    
    AC_ARG_WITH(netcdf-libdir,
                [  --with-netcdf-libdir=<location of NetCDF library>],
                [NETCDF_LIBDIR=$withval
                 with_netcdf=detailed])
    
    AC_ARG_WITH(netcdf-libs,
                [  --with-netcdf-libs=<linker flags besides -L<netcdf_libdir>, e.g. -lnetcdf>],
                [NETCDF_LIBS=$withval
                 with_netcdf=detailed])
    
    dnl If we know NETCDF_DIR, then we can know NETCDF_INCDIR.
    dnl If we know CRAY_NETCDF_DIR, then we leave NETCDF_INCDIR empty.
    dnl We may have NETCDF denoting the dir (e.g. on ewok BUT on franklin it contains all flags)
    dnl We don't overwrite NETCDF_INCDIR.
    if test -z "${NETCDF_INCDIR}"; then
        if test -n "${NETCDF_DIR}"; then
            NETCDF_INCDIR="${NETCDF_DIR}/include";
        elif test -n "${CRAY_NETCDF_DIR}"; then
            NETCDF_INCDIR="";
            ac_use_cray_netcdf=yes
        elif test -n "${NETCDF}" -a -d "${NETCDF}"; then
            NETCDF_INCDIR="${NETCDF}/include"
        fi
    fi
    
    dnl If we know NETCDF_DIR, then we can know NETCDF_LIBDIR.
    dnl If we know CRAY_NETCDF_DIR, then we leave NETCDF_LIBDIR empty.
    dnl We may have NETCDF denoting the dir (e.g. on ewok BUT on franklin it contains all flags)
    dnl We don't overwrite NETCDF_LIBDIR.
    if test -z "${NETCDF_LIBDIR}"; then
        if test -n "${NETCDF_DIR}"; then
            NETCDF_LIBDIR="${NETCDF_DIR}/lib";
        elif test -n "${CRAY_NETCDF_DIR}"; then
            NETCDF_LIBDIR="";
            ac_use_cray_netcdf=yes
        elif test -n "${NETCDF}" -a -d "${NETCDF}"; then
            NETCDF_LIBDIR="${NETCDF}/lib"
        fi
    fi
    
    dnl Add "-I" to NETCDF_INCDIR.
    if test -n "${NETCDF_INCDIR}"; then
            NETCDF_CPPFLAGS="-I${NETCDF_INCDIR}"
    else
            ac_netcdf_ok=no
    fi

    dnl Add "-L" to NETCDF_LIBDIR.
    if test -n "${NETCDF_LIBDIR}"; then
            NETCDF_LDFLAGS="-L${NETCDF_LIBDIR}"
    else
            ac_netcdf_ok=no
    fi

    dnl if netcdf libs are not defined (and not Cray netcdf lib), then guess and define it
    if test -z "${NETCDF_LIBS}"; then
        if test "${ac_use_cray_netcdf}" != "yes"; then
            NETCDF_LIBS="-lnetcdf"
        fi
    fi
    
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $NETCDF_LDFLAGS"
    LDFLAGS="$LDFLAGS $NETCDF_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $NETCDF_CPPFLAGS"
    
    if test -z "${HAVE_NETCDF_TRUE}"; then
        AC_CHECK_HEADERS(netcdf.h,
           ,
           [if test "x$with_netcdf" != xcheck; then
              AC_MSG_FAILURE( [--with-netcdf was given, but test for netcdf.h failed])
            fi
            AM_CONDITIONAL(HAVE_NETCDF,false)])
    fi
    
    if test -z "${HAVE_NETCDF_TRUE}"; then
        AC_MSG_CHECKING([if netcdf code can be compiled])
        AC_TRY_COMPILE([#include "netcdf.h"],
            [int ncid;
             nc_create("a.nc", NC_CLOBBER, &ncid);
             nc_close(ncid);
            ],
            [AC_MSG_RESULT(yes)],
            [AC_MSG_RESULT(no)
             if test "x$with_netcdf" != xcheck; then
                 AC_MSG_FAILURE( [--with-netcdf was given, but compile test failed])
             fi
             AM_CONDITIONAL(HAVE_NETCDF,false)
            ])
    
        AC_SUBST(NETCDF_LIBS)
        AC_SUBST(NETCDF_LDFLAGS)
        AC_SUBST(NETCDF_CPPFLAGS)
    fi
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    
    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_NETCDF_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_NETCDF,1,[Define if you have NETCDF.])],[$1])
            :
    else
            $2
            :
    fi

fi

])
