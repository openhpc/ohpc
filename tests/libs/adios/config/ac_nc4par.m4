dnl ######################################################################
dnl
dnl Finds netCDF 4 parallel
dnl
dnl ######################################################################

AC_DEFUN([AC_NC4PAR],
[
AC_MSG_NOTICE([=== checking NetCDF 4 Parallel ===])

AM_CONDITIONAL(HAVE_NC4PAR,true)

AC_ARG_WITH(nc4par,
            [  --with-nc4par=<location of NetCDF 4 Parallel installation>],
            [NC4PAR_DIR=$withval], [with_nc4par=no])

dnl If --without-nc4par was given set HAVE_NC4PAR to false and do nothing more
dnl Or if nothing was given, by default we don't try to find it anymore
if test "x$with_nc4par" == "xno"; then

   AM_CONDITIONAL(HAVE_NC4PAR,false)

else

    ac_use_cray_netcdf=no

    dnl allow args --with-nc4par incdir and --with-nc4par-libdir
    AC_ARG_WITH(nc4par-incdir,
                [  --with-nc4par-incdir=<location of NetCDF 4 Parallel includes>],
                [NC4PAR_INCDIR=$withval
                 with_nc4par=detailed])
    
    AC_ARG_WITH(nc4par-libdir,
                [  --with-nc4par-libdir=<location of NetCDF 4 Parallel library>],
                [NC4PAR_LIBDIR=$withval
                 with_nc4par=detailed])
    
    AC_ARG_WITH(nc4par-libs,
                [  --with-nc4par-libs=<linker flags besides -L<nc4par_libdir>, e.g. -lnetcdf>],
                [NC4PAR_LIBS=$withval
                 with_nc4par=detailed])
    
    
    dnl If we know NC4PAR_DIR, then we can know NC4PAR_INCDIR.
    dnl If we know CRAY_NETCDF_DIR, then we leave NC4PAR_INCDIR empty.
    dnl Or, if we know NETCDF_DIR, then we can know NC4PAR_INCDIR.
    dnl We don't overwrite NC4PAR_INCDIR.
    if test -z "${NC4PAR_INCDIR}"; then
        if test -n "${NC4PAR_DIR}"; then
            NC4PAR_INCDIR="${NC4PAR_DIR}/include";
        elif test -n "${CRAY_NETCDF_DIR}"; then
            NC4PAR_INCDIR="";
            ac_use_cray_netcdf=yes
        elif test -n "${NETCDF_DIR}"; then
            NC4PAR_INCDIR="${NETCDF_DIR}/include";
        fi
    fi
    
    dnl If we know NC4PAR_DIR, then we can know NC4PAR_LIBDIR.
    dnl If we know CRAY_NETCDF_DIR, then we leave NC4PAR_LIBDIR empty.
    dnl Or, if we know NETCDF_DIR, then we may know NC4PAR_LIBDIR.
    dnl We don't overwrite NC4PAR_LIBDIR.
    if test -z "${NC4PAR_LIBDIR}"; then
        if test -n "${NC4PAR_DIR}"; then
            NC4PAR_LIBDIR="${NC4PAR_DIR}/lib";
        elif test -n "${CRAY_NETCDF_DIR}"; then
            NC4PAR_LIBDIR="";
            ac_use_cray_netcdf=yes
        elif test -n "${NETCDF_DIR}"; then
            NC4PAR_LIBDIR="${NETCDF_DIR}/lib"
        fi
    fi
    
    dnl Add "-I" to NC4PAR_INCDIR.
    if test -n "${NC4PAR_INCDIR}"; then
            NC4PAR_CPPFLAGS="-I${NC4PAR_INCDIR}"
    else
            ac_nc4par=no
    fi
    
    dnl Add "-L" to NC4PAR_LIBDIR + NC4PAR_LIBS.
    if test -n "${NC4PAR_LIBDIR}"; then
            NC4PAR_LDFLAGS="-L${NC4PAR_LIBDIR}"
    else
            ac_nc4par=no
    fi
    
    dnl if nc4 libs are not defined (and not Cray nc4 lib), then guess and define it
    if test -z "${NC4PAR_LIBS}"; then
        if test "${ac_use_cray_netcdf}" != "yes"; then
            NC4PAR_LIBS="-lnetcdf"
        fi
    fi

    save_CC="$CC"
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $NC4PAR_LIBS"
    LDFLAGS="$LDFLAGS $NC4PAR_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $NC4PAR_CPPFLAGS"
    CC="$MPICC"
    
    if test -z "${HAVE_NC4PAR_TRUE}"; then
        AC_CHECK_HEADERS(netcdf.h,
           ,
           [if test "x$with_nc4par" != xcheck; then
              AC_MSG_FAILURE( [--with-nc4par was given, but test for netcdf.h failed])
            fi
            AM_CONDITIONAL(HAVE_NC4PAR,false)])
    fi
    
    if test -z "${HAVE_NC4PAR_TRUE}"; then
        AC_MSG_CHECKING([if nc4 parallel code can be compiled])
        AC_TRY_COMPILE(
            [#include "mpi.h"
             #include "netcdf.h"
            ],
            [int ncid;
             MPI_Info info;
             MPI_Comm comm;
             nc_create_par("a.nc", NC_NOCLOBBER|NC_MPIIO|NC_NETCDF4, comm, info, ncid);
             nc_close(ncid);
            ],
            [AC_MSG_RESULT(yes)],
            [AC_MSG_RESULT(no)
             if test "x$with_nc4par" != xcheck; then
                 AC_MSG_FAILURE( [--with-nc4par was given, but compile test failed])
             fi
             AM_CONDITIONAL(HAVE_NC4PAR,false)
            ])
    
        AC_SUBST(NC4PAR_LIBS)
        AC_SUBST(NC4PAR_LDFLAGS)
        AC_SUBST(NC4PAR_CPPFLAGS)
    fi
    
    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    CC="$save_CC"
    
    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_NC4PAR_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_NC4PAR,1,[Define if you have NC4PAR.])],[$1])
            :
    else
            $2
            :
    fi

fi

])
