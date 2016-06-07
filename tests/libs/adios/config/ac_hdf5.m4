dnl ######################################################################
dnl
dnl Finds HDF5
dnl
dnl ######################################################################

AC_DEFUN([AC_HDF5],
[
AC_MSG_NOTICE([=== checking for HDF5 ===])

AM_CONDITIONAL(HAVE_HDF5,true)

dnl Automatic checking for HDF5 is disabled now.
AC_ARG_WITH(hdf5,
            [  --with-hdf5=<location of HDF5 installation>],
            [HDF5_DIR=$withval], [with_hdf5=no])

AC_ARG_WITH(hdf5-incdir,
            [  --with-hdf5-incdir=<location of HDF5 includes>],
            [HDF5_INCDIR=$withval
             with_hdf5=detailed])

AC_ARG_WITH(hdf5-libdir,
             [  --with-hdf5-libdir=<location of HDF5 library>],
             [HDF5_LIBDIR=$withval
              with_hdf5=detailed])

AC_ARG_WITH(hdf5-libs,
             [  --with-hdf5-libs=<linker flags besides -L<hdf5_libdir>, e.g. -lhdf5 -lhdf5_hl -lz>],
             [HDF5_LIBS=$withval
              with_hdf5=detailed])


dnl If --without-hdf5 was given set HAVE_HDF5 to false and do nothing more
dnl If nothing was given, then too, do nothing 
if test "x$with_hdf5" == "xno"; then

   AM_CONDITIONAL(HAVE_HDF5,false)

else

    dnl If we know HDF5_DIR, then we can know HDF5_INCDIR.
    dnl We don't overwrite HDF5_INCDIR.
    if test -z "${HDF5_INCDIR}"; then
        if test -n "${HDF5_DIR}"; then
            HDF5_INCDIR="${HDF5_DIR}/include";
        fi
    fi

    dnl If we know HDF5_DIR, then we can know HDF5_LIBDIR.
    dnl We don't overwrite HDF5_LIBDIR.
    if test -z "${HDF5_LIBDIR}"; then
        if test -n "${HDF5_DIR}"; then
            HDF5_LIBDIR="${HDF5_DIR}/lib";
        fi
    fi

    dnl Add "-I" to HDF5_INCDIR.
    HDF5_CPPFLAGS="-I${HDF5_INCDIR}"

    dnl Add "-L" to HDF5_LIBDIR.
    HDF5_LDFLAGS="-L${HDF5_LIBDIR}"

    dnl if hdf5 libs are not defined then guess and define it
    if test -z "${HDF5_LIBS}"; then
        dnl default HDF5 lib is usually just -lhdf5 -lz
        HDF5_LIBS="-lhdf5_hl -lhdf5 -lz"
    fi

    save_CC="$CC"
    save_CPPFLAGS="$CPPFLAGS"
    save_LIBS="$LIBS"
    save_LDFLAGS="$LDFLAGS"
    LIBS="$LIBS $HDF5_LIBS"
    LDFLAGS="$LDFLAGS $HDF5_LDFLAGS"
    CPPFLAGS="$CPPFLAGS $HDF5_CPPFLAGS"
    CC="$MPICC"

    if test -z "${HAVE_HDF5_TRUE}"; then
        dnl  AC_CHECK_HEADERS(hdf5.h,
        dnl      ,
        dnl      [if test "x$with_hdf5" != xcheck; then
        dnl          AC_MSG_FAILURE( [--with-hdf5 was given, but test for hdf5.h failed])
        dnl       fi
        dnl       AM_CONDITIONAL(HAVE_HDF5,false)])
        AC_MSG_CHECKING([for hdf5.h])
        if test -f ${HDF5_INCDIR}/hdf5.h; then
            AC_MSG_RESULT(yes)
        else
            AM_CONDITIONAL(HAVE_HDF5,false)
            AC_MSG_RESULT(no)
            AC_MSG_FAILURE( [--with-hdf5 was given, but test for ${HDF5_INCDIR}/hdf5.h failed])
        fi
    fi

    if test -z "${HAVE_HDF5_TRUE}"; then
        AC_MSG_CHECKING([if hdf5 code can be compiled])
        AC_TRY_COMPILE([#include "hdf5.h"],
            [hid_t file_id;
             herr_t status;
             file_id = H5Fcreate("a.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
             status = H5Fclose(file_id);
            ],
            [AC_MSG_RESULT(yes)],
            [AC_MSG_RESULT(no)
             if test "x$with_hdf5" != xcheck; then
                AC_MSG_FAILURE( [--with-hdf5 was given, but compile test failed])
             fi
             AM_CONDITIONAL(HAVE_HDF5,false)
            ])

        AC_SUBST(HDF5_LIBS)
        AC_SUBST(HDF5_LDFLAGS)
        AC_SUBST(HDF5_CPPFLAGS)
    fi

    LIBS="$save_LIBS"
    LDFLAGS="$save_LDFLAGS"
    CPPFLAGS="$save_CPPFLAGS"
    CC="$save_CC"

    # Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
    if test -z "${HAVE_HDF5_TRUE}"; then
            ifelse([$1],,[AC_DEFINE(HAVE_HDF5,1,[Define if you have HDF5.])],[$1])
            :
    else
            $2
            :
    fi

fi

])
